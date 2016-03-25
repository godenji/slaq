package org.scalaquery.ql

import org.scalaquery.SQueryException
import org.scalaquery.util.{Node, BinaryNode, WithOp, ValueLinearizer}
import scala.annotation.unchecked.{uncheckedVariance=> uV}

/**
 * A query monad which contains the AST for a query's projection and 
 * the accumulated restrictions and other modifiers.
 */
sealed abstract class Query[+P,+U] extends Node {
	override def toString = "Query"
	
	val reified: Node
	val linearizer: ValueLinearizer[_ <: U]
	val unpackable: Unpackable[_ <: P, _ <: U] 
	val cond: List[Column[_]]
	val modifiers: List[QueryModifier]

  def nodeChildren = reified :: cond.map(Node.apply) ::: modifiers
  override def nodeNamedChildren = 
  	(reified, "select") :: 
  	cond.map(n=> (Node(n), "where")) ::: 
  	modifiers.map(o=> (o, "modifier"))

  def flatMap[P2,U2](f: P=> Query[P2,U2]): Query[P2,U2] = {
    val q = f(unpackable.value)
    new QueryWrap[P2,U2](
    	q.unpackable, cond ::: q.cond, modifiers ::: q.modifiers
    )
  }
  def >>[P2,U2](q: Query[P2,U2]): Query[P2,U2] = flatMap(_=> q)

  def map[P2,U2](f: P=> P2)(implicit unpack: Unpack[P2,U2]): 
  	Query[P2,U2] = {
  		flatMap{p=> Query(f(p))}
  	}

  def filter[T](f: P=> T)(implicit qc: Queryable[T]): Query[P,U] =
    new QueryWrap[P,U](
    	unpackable, qc(f(unpackable.value), cond), modifiers
    )

  def withFilter[T](f: P=> T)(implicit qc: Queryable[T]): 
  	Query[P,U] = filter(f)(qc)

  def groupBy(by: Column[_]*) =
    new QueryWrap[P,U](
    	unpackable, cond,
    	modifiers ::: by.view.map(c => new Grouping(Node(c))).toList
    )
  
  def having[T <: Column[_]](f: P=> T)(implicit qc: Queryable[T]): Query[P,U] = {
  	val c = f(unpackable.value)
  	val conditions = qc(c, cond).map{
  		case x if x == c => HavingColumn(c)(c.typeMapper)
  		case x => x
  	}
    new QueryWrap[P,U](unpackable, conditions, modifiers)
  }

  def orderBy(by: Ordering*) = new QueryWrap[P,U](
  	unpackable, cond, modifiers ::: by.toList
  )

  def exists = 
  	StdFunction[Boolean]("exists", map(_ => ConstColumn(1)))
  
  def take(num: Int): Query[P,U] = take( ConstColumn(num) )
  def drop(num: Int): Query[P,U] = drop( ConstColumn(num) )
  
  def take(node: Column[Int]): Query[P,U] = 
  	takeDrop(
  		TakeDrop.extract[TakeDrop](modifiers) {
		    case Some(TakeDrop(None,d,_)) => TakeDrop( Some(node), d, compareNode = d )
		    case _ => 											 TakeDrop( Some(node), None )
		  }
  	)
  def drop(node: Column[Int]): Query[P,U] = 
  	takeDrop(
  		TakeDrop.extract[TakeDrop](modifiers) {
		    case Some(TakeDrop(t,None,_)) => TakeDrop( t, Some(node), compareNode = t )
		    case _ => 											 TakeDrop( None, Some(node) )
		  }
  	)
  private def takeDrop(x: (TakeDrop, List[QueryModifier])): Query[P,U] = 
  	new QueryWrap[P,U](
  		unpackable, cond, x._1 :: x._2
  	)

//  def union1[O >: P, R](other: Query[O, U]) = {
////  	val uu = Union2(
////  		Node(unpackable.value), Node(other.unpackable.value), false
////  	)
//  	val uu = Union2(this, other, false)
//    new QueryWrap[O, U]( 
//    	unpackable, cond, modifiers
//    )
//  }
//    
//  def union2[O >: P, T >: U, R](other: Query[O, T]*)
//  	(implicit reify: Reify[O, R]) = wrap(Union(false, this :: other.toList))
  
  def union[O >: P, T >: U, R](other: Query[O, T]*)
  	(implicit reify: Reify[O, R]) = wrap(Union(false, this :: other.toList))

  def unionAll[O >: P, T >: U, R](other: Query[O, T]*)
  	(implicit reify: Reify[O, R]) = wrap(Union(true, this :: other.toList))

  def sub[UU >: U, R](implicit reify: Reify[P, R]) = wrap(this)

  private def wrap[R](base: Node)(implicit reify: Reify[P, R]): Query[R, U] = {
    def f[PP](unpackable: Unpackable[PP, _ <: U]) = 
    	unpackable.endoMap(v=> v match {
	      case t:Table[_] =>
	        t.mapOp(_ => Subquery(base, false)).asInstanceOf[PP]
	      case o =>
	        var pos = 0
	        val p = Subquery(base, true)
	        unpackable.mapOp{v=>
	          pos += 1
	          SubqueryColumn(pos, p, v match {
	            case c: Column[_] => c.typeMapper
	            case SubqueryColumn(_,_,tm) => tm
	            case _ => throw new SQueryException(
	            	"Expected Column or SubqueryColumn"
	            )
	          })
	        }
	    })
    val r: Unpackable[R, _ <: U] = unpackable.reifiedUnpackable(reify)
    Query[R, U](f(r))
  }

  def asColumn(implicit ev: P <:< Column[_]): P =
  	ev(unpackable.value).mapOp(_=> this).asInstanceOf[P]
}

object Query extends QueryWrap
	[Unit,Unit](Unpackable((), Unpack.unpackPrimitive[Unit]), Nil, Nil) {
	
  def apply[P,U](value: P)(implicit unpack: Unpack[P,U]): Query[P,U] =
  	wrapper(Unpackable(value, unpack))
  	
  def apply[P,U](unpackable: Unpackable[_ <: P, _ <: U]): Query[P,U] =
  	wrapper(unpackable)
	
	private def wrapper[P,U](unpack: Unpackable[P,U]) =
		new QueryWrap[P,U](unpack, Nil, Nil)
}

class QueryWrap[+P,+U](
	val unpackable: Unpackable[_ <: P, _ <: U], 
	val cond: List[Column[_]],
	val modifiers: List[QueryModifier]
) extends Query[P,U] {
	
	override lazy val reified = unpackable.reifiedNode
  override lazy val linearizer = unpackable.linearizer
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

//case class Union2(left: Query[_,_], right: Query[_,_], all: Boolean) 
//	extends BinaryNode {
//  	override def toString = if(all) "Union all" else "Union"
//	}

