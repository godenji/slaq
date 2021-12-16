package slaq.ql

import slaq.util.Node
import slaq.Fail

case class Subquery(query: Node, rename: Boolean) extends Node {
  def nodeChildren = query :: Nil
  override def nodeNamedChildren = (query, "query") :: Nil
  override def isNamedTable = true
}

case class SubqueryColumn(
  pos: Int, subquery: Subquery, typeMapper: TypeMapper[_]
) extends Node {

  def nodeChildren = subquery :: Nil
  override def nodeNamedChildren = (subquery, "subquery") :: Nil
  override def toString = s"SubqueryColumn c$pos"
}

case class Union(all: Boolean, queries: List[Query[_, _]]) extends Node {
  override def toString = if (all) "Union all" else "Union"
  def nodeChildren = queries
}

object Subquery {
  def query[P, U, R](query: Query[P, U])(unpackable: Unpackable[_ <: P, _ <: U], reify: Reify[P, R]) = {
    val r: Unpackable[R, _ <: U] = unpackable.reifiedUnpackable(using reify)
    Query[R, U](f(r, query))
  }

  def union[P, U, O >: P, T >: U, R](left: Query[P, U], right: Query[O, T], all: Boolean)(unpackable: Unpackable[_ <: P, _ <: U], reify: Reify[P, R]): Query[R, U] = {
    val r: Unpackable[R, _ <: U] = unpackable.reifiedUnpackable(using reify)
    Query[R, U](f(r, Union(all, List(left, right))))
  }

  private def f[PP, U](unpackable: Unpackable[PP, _ <: U], node: Node) = {
    Unpackable(
      unpackable.value match {
        case t: Table[_] =>
          t.mapOp(_ => Subquery(node, false)).asInstanceOf[PP]
        case _ =>
          val p = Subquery(node, true)
          var pos = 0
          unpackable.mapOp { n =>
            pos += 1; SubqueryColumn(pos, p, mapper(n))
          }
      }, unpackable.unpack
    )
  }

  private def mapper(n: Node) = n match {
    case c: Column[_] => c.typeMapper
    case SubqueryColumn(_, _, tm) => tm
    case x => Fail(s"Expected Column or SubqueryColumn but got $x")
  }
}
