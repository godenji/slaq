package org.scalaquery.ql

import org.scalaquery.SQueryException
import org.scalaquery.util.{Node, WithOp, ValueLinearizer}
import scala.annotation.unchecked.{uncheckedVariance=> uV}

sealed abstract class Query[+P,+U] extends Node {
	override def toString = "Query"
	
	def reified: Node
	def linearizer: ValueLinearizer[_ <: U]
	def unpackable: Unpackable[_ <: P, _ <: U] 
	def cond: List[Column[_]]
	def modifiers: List[QueryModifier]

  def nodeChildren = reified :: cond.map(Node.apply) ::: modifiers
  override def nodeNamedChildren = 
  	(reified, "select") :: cond.map(n=> (Node(n), "where")) ::: 
  	modifiers.map(o=> (o, "modifier"))

  def flatMap[P2,U2](f: P => Query[P2,U2]): Query[P2,U2] = {
    val q = f(unpackable.value)
    new QueryWrap[P2,U2](
    	q.unpackable, cond ::: q.cond, modifiers ::: q.modifiers
    )
  }
  def >>[P2,U2](q: Query[P2,U2]): Query[P2,U2] = flatMap(_ => q)

  def map[P2,U2](f: P => P2)(implicit unpack: Unpack[P2,U2]): 
  	Query[P2,U2] = flatMap{
  		p=> Query(f(p))(unpack)
  	}

  def filter[T](f: P => T)(implicit qc: Queryable[T]): Query[P,U] =
    new QueryWrap[P,U](
    	unpackable, qc(f(unpackable.value), cond), modifiers
    )
  def withFilter[T](f: P => T)
  	(implicit qc: Queryable[T]): Query[P,U] = filter(f)(qc)

  def groupBy(by: Column[_]*) =
    new QueryWrap[P,U](
    	unpackable, cond,
    	modifiers ::: by.view.map(c => new Grouping(Node(c))).toList
    )

  def orderBy(by: Ordering*) = new QueryWrap[P,U](
  	unpackable, cond, modifiers ::: by.toList
  )
  
  def having[T <: Column[_]](f: P => T)(implicit qc: Queryable[T]): Query[P,U] = {
  	val c = f(unpackable.value)
  	val conditions = qc(c, cond).map{
  		case x if x == c => HavingColumn(c)(c.typeMapper)
  		case x => x
  	}
    new QueryWrap[P,U](unpackable, conditions, modifiers)
  }
  
  def exists = StdFunction[Boolean]("exists", map(_ => ConstColumn(1)))

  def asColumn(implicit ev: P <:< Column[_]): P =
  	ev(unpackable.value).mapOp(_=> this).asInstanceOf[P]
  
  def take(num: Int): Query[P,U] = take( ConstColumn(num) )
  def drop(num: Int): Query[P,U] = drop( ConstColumn(num) )
  def take(node: Column[Int]): Query[P,U] = takeDrop(node, isTake = true)
  def drop(node: Column[Int]): Query[P,U] = takeDrop(node, isTake = false)
  
  private def takeDrop(node: Column[Int], isTake: Boolean) = {
  	val fn = if(isTake) TakeDrop.take(_,_) else TakeDrop.drop(_,_)
  	val	(mod, others) = fn(modifiers, node)
  	new QueryWrap[P,U](unpackable, cond, mod :: others)
  }
  
  def union[O >: P, T >: U, R](next: Query[O, T])(implicit reify: Reify[O, R]): 
  	Query[R, U] = union(next, false)

  def unionAll[O >: P, T >: U, R](next: Query[O, T])(implicit reify: Reify[O, R]): 
  	Query[R, U] = union(next, true)

  private def union[O >: P, T >: U, R]
  	(next: Query[O, T], all: Boolean)(implicit reify: Reify[P, R]) = {
  	
  	def mapper(n: Node) = n match {
	    case c: Column[_] => c.typeMapper
	    case SubqueryColumn(_,_,tm) => tm
	    case _ => throw new SQueryException("Expected Column or SubqueryColumn")
  	}
    def f[PP](unpackable: Unpackable[PP, _ <: U], node: Node) = Unpackable(
  		unpackable.value match {
	      case t: Table[_] => t.mapOp(_ => Subquery(node, false)).asInstanceOf[PP]
	      case o =>
	        val p = Subquery(node, true) // must be defined outside mapOp
	        var pos = 0
	        unpackable.mapOp{n=>
	          pos += 1; SubqueryColumn(pos, p, mapper(n))
	        }
	    }, unpackable.unpack
	  )
    val r: Unpackable[R, _ <: U] = unpackable.reifiedUnpackable(reify)
    Query[R, U]( f(r, Union(all, List(this, next))) )
  }
  
}

class QueryWrap[+P,+U](
	val unpackable: Unpackable[_ <: P, _ <: U], 
	val cond: List[Column[_]],
	val modifiers: List[QueryModifier]) extends Query[P,U] {
	
	override lazy val reified = unpackable.reifiedNode
  override lazy val linearizer = unpackable.linearizer
}

object Query extends QueryWrap
	[Unit,Unit](Unpackable((), Unpack.unpackPrimitive[Unit]), Nil, Nil) {
	
  def apply[P,U](value: P)(implicit unpack: Unpack[P,U]): 
  	Query[P,U] = wrapper(Unpackable(value, unpack))
  	
  def apply[P,U](unpackable: Unpackable[_ <: P, _ <: U]): 
  	Query[P,U] = wrapper(unpackable)
	
	private def wrapper[P,U](unpack: Unpackable[P,U]) =
		new QueryWrap[P,U](unpack, Nil, Nil)
}

case class Subquery(query: Node, rename: Boolean) extends Node {
  def nodeChildren = query :: Nil
  override def nodeNamedChildren = (query, "query") :: Nil
  override def isNamedTable = true
}

case class SubqueryColumn(
	pos: Int, subquery: Subquery, typeMapper: TypeMapper[_]) extends Node {
	
  def nodeChildren = subquery :: Nil
  override def nodeNamedChildren = (subquery, "subquery") :: Nil
  override def toString = s"SubqueryColumn c$pos"
}

case class Union(all: Boolean, queries: List[Query[_,_]]) extends Node {
  override def toString = if(all) "Union all" else "Union"
  def nodeChildren = queries
}

