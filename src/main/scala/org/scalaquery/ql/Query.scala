package org.scalaquery.ql

import scala.reflect.ClassTag
import org.scalaquery.SQueryException
import org.scalaquery.util.{Node, WithOp, ValueLinearizer}

/**
 * A query monad which contains the AST for a query's projection and the accumulated
 * restrictions and other modifiers.
 */
sealed abstract class Query[+E,+V] extends Node {
	val reified: Node
	val linearizer: ValueLinearizer[_ <: V]
	val unpackable: Unpackable[_ <: E, _ <: V] 
	val cond: List[Column[_]]
	val condHaving: List[Column[_]]
	val modifiers: List[QueryModifier]

  def nodeChildren = reified :: cond.map(Node.apply) ::: modifiers
  override def nodeNamedChildren = (reified, "select") :: cond.map(n => (Node(n), "where")) ::: modifiers.map(o => (o, "modifier"))

  override def toString = "Query"

  def flatMap[F,T](f: E => Query[F,T]): Query[F,T] = {
    val q = f(unpackable.value)
    new QueryWrap[F,T](q.unpackable, cond ::: q.cond, condHaving ::: q.condHaving, modifiers ::: q.modifiers)
  }

  def map[F,T](f: E => F)(implicit unpack: Unpack[F,T]): Query[F,T] = { 
  	flatMap{v=> Query(f(v))}
  }

  // append operation (e.g. orderBy chaining)
  def >>[F, T](q: Query[F, T]): Query[F, T] = flatMap(_ => q)

  def filter[T](f: E => T)(implicit wt: CanBeQueryCondition[T]): Query[E, V] =
    new QueryWrap[E, V](unpackable, wt(f(unpackable.value), cond), condHaving, modifiers)

  def withFilter[T](f: E => T)(implicit wt: CanBeQueryCondition[T]): Query[E, V] = 
  	filter(f)(wt)

  def where[T <: Column[_]](f: E => T)(implicit wt: CanBeQueryCondition[T]): Query[E, V] = 
  	filter(f)(wt)

  def having[T <: Column[_]](f: E => T)(implicit wt: CanBeQueryCondition[T]): Query[E, V] =
    new QueryWrap[E, V](unpackable, cond, wt(f(unpackable.value), condHaving), modifiers)

  def groupBy(by: Column[_]*) =
    new QueryWrap[E, V](unpackable, cond, condHaving, modifiers ::: by.view.map(c => new Grouping(Node(c))).toList)

  def orderBy(by: Ordering*) = 
  	new QueryWrap[E, V](unpackable, cond, condHaving, modifiers ::: by.toList)

  def exists = 
  	StdFunction[Boolean]("exists", map(_ => ConstColumn(1)))

  def typedModifiers[T <: QueryModifier](implicit m: ClassTag[T]) =
    modifiers.filter(m.runtimeClass.isInstance(_)).asInstanceOf[List[T]]

  def createOrReplaceSingularModifier[T <: QueryModifier](f: Option[T] => T)(implicit m: ClassTag[T]): Query[E, V] = {
    val (xs, other) = modifiers.partition(m.runtimeClass.isInstance(_))
    val mod = xs match {
      case x :: _ => f(Some(x.asInstanceOf[T]))
      case _ => f(None)
    }
    new QueryWrap[E, V](unpackable, cond, condHaving, mod :: other)
  }
  
  def take(num: Int): Query[E,V] = take(ConstColumn(num))
  def drop(num: Int): Query[E,V] = drop(ConstColumn(num))
  
  def take(node: Column[Int]): Query[E,V] = createOrReplaceSingularModifier[TakeDrop] {
    case Some(TakeDrop(None,d,_)) => TakeDrop(Some(node),d)
    case _ => TakeDrop(Some(node),None)
  }
  def drop(node: Column[Int]): Query[E,V] = createOrReplaceSingularModifier[TakeDrop] {
    case Some(TakeDrop(t,None,_)) => TakeDrop(t, Some(node), compareNode = Some(node))
    case _ => TakeDrop(None,Some(node))
  }
  
  def join[E2,V2](q: Query[E2,V2], joinType: Join.JoinType = Join.Inner) = {
  	new JoinBase[E,E2](unpackable.value, q.unpackable.value, joinType)
  }
  def leftJoin[E2,V2](q: Query[E2,V2]) = join(q, Join.Left)
  def rightJoin[E2,V2](q: Query[E2,V2]) = join(q, Join.Right)
  def outerJoin[E2,V2](q: Query[E2,V2]) = join(q, Join.Outer)

  def union[O >: E, T >: V, R](other: Query[O, T]*)(implicit reify: Reify[O, R]) = 
  	wrap(Union(false, this :: other.toList))

  def unionAll[O >: E, T >: V, R](other: Query[O, T]*)(implicit reify: Reify[O, R]) = 
  	wrap(Union(true, this :: other.toList))

  def count = 
  	ColumnOps.CountAll(Subquery(this, false))

  def sub[UU >: V, R](implicit reify: Reify[E, R]) = wrap(this)

  private def wrap[R](base: Node)(implicit reify: Reify[E, R]): Query[R, V] = {
    def f[EE](unpackable: Unpackable[EE, _ <: V]) = unpackable.endoMap(v => v match {
      case t:AbstractTable[_] =>
        t.mapOp(_ => Subquery(base, false)).asInstanceOf[EE]
      case o =>
        var pos = 0
        val p = Subquery(base, true)
        unpackable.mapOp { v =>
          pos += 1
          SubqueryColumn(pos, p, v match {
            case c: Column[_] => c.typeMapper
            case SubqueryColumn(_, _, tm) => tm
            case _ => throw new SQueryException("Expected Column or SubqueryColumn")
          })
        }
    })
    val r: Unpackable[R, _ <: V] = unpackable.reifiedUnpackable(reify)
    Query[R, V](f(r))
  }

  //def reify[R](implicit reify: Reify[E, R]) =
  //  new QueryWrap[R, V](unpackable.reifiedUnpackable, cond, condHaving, modifiers)

  def asColumn(implicit ev: E <:< Column[_]): E = 
  	unpackable.value.asInstanceOf[WithOp].mapOp(_ => this).asInstanceOf[E]
}

object Query extends QueryWrap[Unit, Unit](Unpackable((), Unpack.unpackPrimitive[Unit]), Nil, Nil, Nil) {
  def apply[E,V](value: E)(implicit unpack: Unpack[E, V]): Query[E,V] = 
  	new QueryWrap[E, V](Unpackable(value, unpack), Nil, Nil, Nil)
  	
  def apply[E,V](unpackable: Unpackable[_ <: E, _ <: V]): Query[E,V] = 
  	new QueryWrap[E,V](unpackable, Nil, Nil, Nil)
}

class QueryWrap[+E,+V](
	val unpackable: Unpackable[_ <: E, _ <: V], 
	val cond: List[Column[_]],  
	val condHaving: List[Column[_]],
	val modifiers: List[QueryModifier]) extends Query[E,V] {
	
	override lazy val reified = unpackable.reifiedNode
  override lazy val linearizer = unpackable.linearizer
}

trait CanBeQueryCondition[-T] {
  def apply(value: T, l: List[Column[_]]): List[Column[_]]
}

object CanBeQueryCondition {
  implicit object BooleanColumnCanBeQueryCondition extends CanBeQueryCondition[Column[Boolean]] {
    @inline def apply(value: Column[Boolean], l: List[Column[_]]): List[Column[_]] = value :: l
  }
  implicit object BooleanOptionColumnCanBeQueryCondition extends CanBeQueryCondition[Column[Option[Boolean]]] {
    @inline def apply(value: Column[Option[Boolean]], l: List[Column[_]]): List[Column[_]] = value :: l
  }
  implicit object BooleanCanBeQueryCondition extends CanBeQueryCondition[Boolean] {
    @inline def apply(value: Boolean, l: List[Column[_]]): List[Column[_]] =
      if(value) l else new ConstColumn(false)(TypeMapper.BooleanTypeMapper) :: Nil
  }
}

case class Subquery(query: Node, rename: Boolean) extends Node {
  def nodeChildren = query :: Nil
  override def nodeNamedChildren = (query, "query") :: Nil
  override def isNamedTable = true
}

case class SubqueryColumn(pos: Int, subquery: Subquery, typeMapper: TypeMapper[_]) extends Node {
  def nodeChildren = subquery :: Nil
  override def nodeNamedChildren = (subquery, "subquery") :: Nil
  override def toString = "SubqueryColumn c"+pos
}

case class Union(all: Boolean, queries: List[Query[_, _]]) extends Node {
  override def toString = if(all) "Union all" else "Union"
  def nodeChildren = queries
}
