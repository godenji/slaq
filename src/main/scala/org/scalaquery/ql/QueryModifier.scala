package org.scalaquery.ql

import org.scalaquery.util.{Node,NullaryNode}

trait QueryModifier extends Node

sealed abstract class Ordering extends QueryModifier {
  val by: Node
  val nullOrdering: Ordering.NullOrdering
  def nodeChildren = by :: Nil
  def nullsFirst: Ordering
  def nullsLast: Ordering
}

object Ordering {
  final case class Asc(val by: Node, val nullOrdering: Ordering.NullOrdering = Ordering.NullsDefault) extends Ordering {
    override def toString = "Ordering.Asc"
    def nullsFirst = copy(nullOrdering = Ordering.NullsFirst)
    def nullsLast = copy(nullOrdering = Ordering.NullsLast)
  }

  final case class Desc(val by: Node, val nullOrdering: Ordering.NullOrdering = Ordering.NullsDefault) extends Ordering {
    override def toString = "Ordering.Desc"
    def nullsFirst = copy(nullOrdering = Ordering.NullsFirst)
    def nullsLast = copy(nullOrdering = Ordering.NullsLast)
  }

  sealed trait NullOrdering
  final case object NullsDefault extends NullOrdering
  final case object NullsFirst extends NullOrdering
  final case object NullsLast extends NullOrdering
}

final case class Grouping(val by: Node) extends QueryModifier {
  def nodeChildren = by :: Nil
  override def toString = "Grouping"
}

/*
 * @compareNode used to calculate `take x drop y` operation where take must be of value max(0, x-y)
 * @see BasicQueryBuilder `appendColumnValue`
 */
final case class TakeDrop(
	take: Option[Column[Int]], drop: Option[Column[Int]], compareNode: Option[Column[Int]] = None
) extends QueryModifier with NullaryNode
