package slaq.ql

import slaq.Fail
import slaq.ql.core.Profile
import slaq.session.{PositionedResult, PositionedParameters}
import slaq.util.{
  Node,
  UnaryNode,
  WithOp,
  SimpleTypeName,
  ValueLinearizer
}

/**
 * Common base trait for columns, tables, and projections
 */
trait ColumnBase[T] extends Node with ValueLinearizer[T] with WithOp {
  override def nodeDelegate: Node = if (op eq null) this else op.nodeDelegate
}

sealed abstract class Column[T: TypeMapper] extends ColumnBase[T] {
  final val typeMapper = summon[TypeMapper[T]]
  final def getAllColumnTypeMappers = Vector(typeMapper)
  final def getLinearizedNodes = Vector(Node(this))
  def getResult(profile: Profile, rs: PositionedResult): T = {
    val tmd = typeMapper(profile)
    tmd.nextValueOrElse(tmd.zero, rs)
  }
  final def updateResult(profile: Profile, rs: PositionedResult, value: T): Unit =
    typeMapper(profile).updateValue(value, rs)

  final def setParameter(profile: Profile, ps: PositionedParameters, value: Option[T]): Unit = typeMapper(profile).setOption(value, ps)

  def getOr[U](n: => U)(using Option[U] =:= T): Column[U] =
    new WrappedColumn[U](this)(typeMapper.getBaseTypeMapper) {
      override def getResult(profile: Profile, rs: PositionedResult): U =
        typeMapper(profile).nextValueOrElse(n, rs)
    }
  def get[U](using Option[U] =:= T): Column[U] = getOr[U] {
    Fail(s"Read NULL value for column $this")
  }
  final def ~[U](b: Column[U]) = new Projection2[T, U](this, b)
  def ? : Column[Option[T]] = new WrappedColumn(this)(
    typeMapper.createOptionTypeMapper
  )

  def in(e: Query[Column[_], _]) = ColumnOps.In(Node(this), Node(e))
  def notIn(e: Query[Column[_], _]) = ColumnOps.Not(Node(ColumnOps.In(Node(this), Node(e))))
  def count = StdFunction[Long]("count", Node(this))
  def isNull = ColumnOps.Is(Node(this), ConstColumn.NULL)
  def isNotNull = ColumnOps.Not(Node(ColumnOps.Is(Node(this), ConstColumn.NULL)))
  def countDistinct = ColumnOps.CountDistinct(Node(this))
  def asColumnOf[U: TypeMapper]: Column[U] = AsColumnOf[U](Node(this), None)
  def asColumnOfType[U: TypeMapper](typeName: String): Column[U] =
    AsColumnOf[U](Node(this), Some(typeName))

  def asc = new Ordering.Asc(Node(this))
  def desc = new Ordering.Desc(Node(this))
}

/**
 * A column with a constant value which is inserted into an SQL statement as a literal.
 */
case class ConstColumn[T: TypeMapper](value: T) extends Column[T] {
  def nodeChildren = Nil
  override def toString = s"ConstColumn[${SimpleTypeName.forVal(value)}] $value"
  def bind = new BindColumn(value)
}
object ConstColumn {
  def NULL = new ConstColumn[Null](null)(TypeMapper.NullTypeMapper)
}

/**
 * A column with a constant value which gets turned into a bind variable.
 */
case class BindColumn[T: TypeMapper](value: T) extends Column[T] {
  def nodeChildren = Nil
  override def toString = s"BindColumn[${SimpleTypeName.forVal(value)}] $value"
}

/**
 * A parameter from a QueryTemplate which gets turned into a bind variable.
 */
case class ParameterColumn[T: TypeMapper](idx: Int) extends Column[T] {
  def nodeChildren = Nil
  override def toString = s"ParameterColumn $idx"
}

/**
 * A column which gets created as the result of applying an operator.
 */
abstract class OperatorColumn[T: TypeMapper] extends Column[T] {
  protected[this] val leftOperand: Node = Node(this)
}

/**
 * A column representing a group by having clause
 */
case class HavingColumn[T: TypeMapper](val c: Column[_]) extends Column[T] {
  def nodeChildren = Nil
  override def toString = s"Having $c"
}

/**
 * A column representing a case/when/end condition
 */
abstract class CaseColumn[T: TypeMapper](
  val clauses: List[Case.WhenNode],
  val elseClause: Node
) extends Column[T] {
  def nodeChildren = elseClause :: clauses
}

/**
 * A column that can be converted from one sql type to another
 */
case class AsColumnOf[T: TypeMapper](child: Node, typeName: Option[String])
  extends Column[T] with UnaryNode

/**
 * A WrappedColumn can be used to change a column's nullValue.
 */
class WrappedColumn[T: TypeMapper](parent: ColumnBase[_]) extends Column[T] {
  override def nodeDelegate = if (op eq null) Node(parent) else op.nodeDelegate
  def nodeChildren = nodeDelegate :: Nil
}

/**
 * A column which is part of a Table.
 */
final class NamedColumn[T: TypeMapper](
  val table: Node,
  val name: String,
  val options: core.ColumnOption[T, _]*
) extends Column[T] {

  def nodeChildren = table :: Nil
  override def toString = s"NamedColumn $name"
  override def nodeNamedChildren = (table, "table") :: Nil
}
object NamedColumn {
  def unapply[T](n: NamedColumn[T]) = Some(
    (n.table, n.name, n.options)
  )
}
