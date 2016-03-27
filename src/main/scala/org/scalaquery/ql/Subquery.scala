package org.scalaquery.ql

import org.scalaquery.util.Node

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