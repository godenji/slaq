package org.scalaquery.ql.extended

import scala.math.{min, max}
import org.scalaquery.ql._
import org.scalaquery.ql.TypeMapper._
import org.scalaquery.ql.basic._
import org.scalaquery.session.Session
import org.scalaquery.util.{Node,NullaryNode,SQLBuilder}
import org.scalaquery.SQueryException

trait ExtendedProfile extends BasicProfile {
  type ImplicitT <: ExtendedImplicitConversions[_ <: ExtendedProfile]
}

trait ExtendedImplicitConversions[DriverType <: ExtendedProfile] extends BasicImplicitConversions[DriverType] {
  implicit def queryToExtendedQueryOps[E, U](q: Query[E, U]) = new ExtendedQueryOps(q)
  implicit def extendedQueryToDeleteInvoker[T](q: Query[ExtendedTable[T], T]): BasicDeleteInvoker[T] = new BasicDeleteInvoker(q, scalaQueryDriver)
}

class ExtendedQueryOps[E, U](q: Query[E, U]) {
  import ExtendedQueryOps._

  def take(num: Int): Query[E,U] = take(ConstColumn(num))
  def drop(num: Int): Query[E,U] = drop(ConstColumn(num))
  
  def take(node: Column[Int]) = q.createOrReplaceSingularModifier[TakeDrop] {
    case Some(TakeDrop(None,d,_)) => TakeDrop(Some(node),d)
    case _ => TakeDrop(Some(node),None)
  }
  def drop(node: Column[Int]) = q.createOrReplaceSingularModifier[TakeDrop] {
    case Some(TakeDrop(t,None,_)) => TakeDrop(t, Some(node), compareNode = Some(node))
    case _ => TakeDrop(None,Some(node))
  }
}

object ExtendedQueryOps {
	/*
	 * @compareNode used to calculate `take x drop y` operation where take must be of value max(0, x-y)
	 * @see BasicQueryBuilder `appendColumnValue`
	 */
  final case class TakeDrop(
  	take: Option[Column[Int]], drop: Option[Column[Int]], compareNode: Option[Column[Int]] = None
  ) extends QueryModifier with NullaryNode
}

class ExtendedColumnOptions extends BasicColumnOptions {
  val AutoInc = ExtendedColumnOption.AutoInc
}

object ExtendedColumnOptions extends ExtendedColumnOptions

object ExtendedColumnOption {
  case object AutoInc extends ColumnOption[Nothing, ExtendedProfile]
}

abstract class AbstractExtendedTable[T](_schemaName: Option[String], _tableName: String) extends AbstractBasicTable[T](_schemaName, _tableName) {
  type ProfileType <: ExtendedProfile
  override val O: ExtendedColumnOptions = ExtendedColumnOptions
}

abstract class ExtendedTable[T](_schemaName: Option[String], _tableName: String) extends AbstractExtendedTable[T](_schemaName, _tableName) {
  def this(_tableName: String) = this(None, _tableName)
  type ProfileType = ExtendedProfile
}
