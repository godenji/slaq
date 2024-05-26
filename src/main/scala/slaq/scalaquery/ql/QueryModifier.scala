package slaq.ql

import scala.reflect.ClassTag
import slaq.util.{Node, NullaryNode}
import slaq.util.RefId

sealed trait QueryModifier extends Node

sealed abstract class Ordering extends QueryModifier {
  val by: Node
  val nullOrdering: Ordering.NullOrdering
  def nodeChildren = by :: Nil
  def nullsFirst: Ordering
  def nullsLast: Ordering
}

object Ordering {
  enum NullOrdering:
    case NullsDefault, NullsFirst, NullsLast
  export NullOrdering.*

  final case class Asc(
    val by: Node,
    val nullOrdering: NullOrdering = NullsDefault
  ) extends Ordering {
    override def toString = "Ordering.Asc"
    def nullsFirst = copy(nullOrdering = NullsFirst)
    def nullsLast = copy(nullOrdering = NullsLast)
  }

  final case class Desc(
    val by: Node,
    val nullOrdering: NullOrdering = NullsDefault
  ) extends Ordering {
    override def toString = "Ordering.Desc"
    def nullsFirst = copy(nullOrdering = NullsFirst)
    def nullsLast = copy(nullOrdering = NullsLast)
  }
}

final case class Grouping(val by: Node) extends QueryModifier {
  def nodeChildren = by :: Nil
  override def toString = "Grouping"
}

final case class Lateral(ref: RefId[Query[?, ?]], on: Node) extends QueryModifier {
  def nodeChildren = Nil
  override def toString = "Lateral ON"
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
  private def extract[T <: QueryModifier](modifiers: List[QueryModifier])(f: Option[T] => T)(using m: ClassTag[T]): (T, List[QueryModifier]) = {
    modifiers.partition(m.runtimeClass.isInstance(_)) match {
      case (x :: _, other) => (f(Some(x.asInstanceOf[T])), other)
      case (_, other)      => (f(None), other)
    }
  }
}

