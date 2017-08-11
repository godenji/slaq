package org.scalaquery.ql

import scala.reflect.ClassTag
import org.scalaquery.util.{Node, NullaryNode}

sealed trait QueryModifier extends Node
sealed abstract class Ordering extends QueryModifier {
  val by: Node
  val nullOrdering: Ordering.NullOrdering
  def nodeChildren = by :: Nil
  def nullsFirst: Ordering
  def nullsLast: Ordering
}

object Ordering {
  final case class Asc(
    val by: Node,
    val nullOrdering: Ordering.NullOrdering = Ordering.NullsDefault
  ) extends Ordering {
    override def toString = "Ordering.Asc"
    def nullsFirst = copy(nullOrdering = Ordering.NullsFirst)
    def nullsLast = copy(nullOrdering = Ordering.NullsLast)
  }

  final case class Desc(
    val by: Node,
    val nullOrdering: Ordering.NullOrdering = Ordering.NullsDefault
  ) extends Ordering {
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
 * @compareNode used to calculate limit/offset value in take/drop operation
 * @see QueryBuilderClause `appendLimitValue`
 */
final case class TakeDrop(
  take: Option[Column[Int]],
  drop: Option[Column[Int]],
  compareNode: Option[Column[Int]] = None
) extends QueryModifier with NullaryNode

object TakeDrop {

  def take(modifiers: List[QueryModifier], node: Column[Int]) =
    extract[TakeDrop](modifiers) {
      case Some(TakeDrop(None, d, _)) => TakeDrop(Some(node), d, compareNode = d)
      case _ => TakeDrop(Some(node), None)
    }
  def drop(modifiers: List[QueryModifier], node: Column[Int]) =
    extract[TakeDrop](modifiers) {
      case Some(TakeDrop(t, None, _)) => TakeDrop(t, Some(node), compareNode = t)
      case _ => TakeDrop(None, Some(node))
    }

  /*
	 * extracts first modifier of given T and returns it (if exist) along
	 * 	with remaining modifiers as (T, List[Others])
	 */
  private def extract[T <: QueryModifier](modifiers: List[QueryModifier])(f: Option[T] => T)(implicit m: ClassTag[T]): (T, List[QueryModifier]) = {

    modifiers.partition(m.runtimeClass.isInstance(_)) match {
      case (x :: _, other) => (f(Some(x.asInstanceOf[T])), other)
      case (_, other)      => (f(None), other)
    }
  }
}

