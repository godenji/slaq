package slaq.ql

import slaq.util.{Node, ProductNode}
import slaq.Fail

case class Subquery
  (query: Node, rename: Boolean, maybeJoin: Option[JoinType]) extends Node {

  def nodeChildren = query :: Nil
  override def nodeNamedChildren = (query, "query") :: Nil
  override def isNamedTable = true
}

case class SubqueryColumn(
  pos: Int,
  subquery: Subquery,
  typeMapper: TypeMapper[?],
  maybeNamed: Option[NamedColumn[?]]
) extends Node {

  def nodeChildren = subquery :: Nil
  override def nodeNamedChildren = (subquery, "subquery") :: Nil
  override def toString = s"SubqueryColumn c$pos"
}

case class SubqueryTable(cols: List[SubqueryColumn]) extends Node {
  def nodeChildren = cols
  override def nodeNamedChildren = (this, "subquery table") :: Nil
  override def toString = s"SubqueryTable $cols"
}

case class Union(all: Boolean, queries: List[Query[?, ?]]) extends Node {
  override def toString = if (all) "Union all" else "Union"
  def nodeChildren = queries
}

object Subquery {
  def query[P, U, R]
    (query: Query[P, U], maybeJoin: Option[JoinType])
    (unpackable: Unpackable[? <: P, ? <: U], reify: Reify[P, R]) = {
    val r: Unpackable[R, ? <: U] = unpackable.reifiedUnpackable(using reify)
    Query[R, U](f(r, query, maybeJoin))
  }

  def union[P, U, O >: P, T >: U, R](left: Query[P, U], right: Query[O, T], all: Boolean)(unpackable: Unpackable[? <: P, ? <: U], reify: Reify[P, R]): Query[R, U] = {
    val r: Unpackable[R, ? <: U] = unpackable.reifiedUnpackable(using reify)
    Query[R, U](f(r, Union(all, List(left, right)), None))
  }

  private def f[PP, U]
    (unpackable: Unpackable[PP, ? <: U], node: Node, maybeJoin: Option[JoinType]) = {
    Unpackable(
      unpackable.value match {
        case t: Table[_] =>
          t.mapOp(_ => Subquery(node, false, maybeJoin)).asInstanceOf[PP]
        case _ =>
          val p = Subquery(node, true, maybeJoin)
          var pos = 0
          unpackable.mapOp { n =>
            pos += 1
            n match
              case t: Table[_] =>
                val xs = t.*.nodeChildren.collect {
                  case pn: ProductNode =>
                    pn.nodeChildren.zipWithIndex.collect { case (n2: NamedColumn[_], i) =>
                      if i != 0 then pos += 1
                      SubqueryColumn(pos, p, mapper(n2), Some(n2))
                    }
                }.flatten
                t.mapOp(_ => SubqueryTable(xs))
              case _ =>
                SubqueryColumn(pos, p, mapper(n), None)
          }
      }, unpackable.unpack
    )
  }

  private def mapper(n: Node) = n match {
    case c: Column[_] => c.typeMapper
    case SubqueryColumn(_, _, tm, _) => tm
    case x => Fail(s"""
Expected Column, SubqueryColumn, or Table but got $x -- maybe you tried to yield `t.*`?
See UnionTest.scala for detailed example of union queries with table projections.
    """
    )
  }
}
