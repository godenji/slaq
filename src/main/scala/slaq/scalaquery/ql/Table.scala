package slaq.ql

import slaq.Fail
import slaq.session.{PositionedResult, PositionedParameters}
import slaq.util.{Node, UnaryNode}
import core.{Profile, ColumnOption, ColumnOptions}

abstract class Table[T](
  val schemaName: Option[String],
  val tableName: String
) extends ColumnBase[T] {

  final type TableType = T

  def this(_tableName: String) = this(None, _tableName)

  def nodeChildren = Nil
  override def isNamedTable = true
  override def toString = s"Table $tableName"

  final def maybeJoin: Option[Join] = nodeDelegate match {
    case j: Join => Some(j)
    case _       => None
  }

  type ProfileType = Profile
  val O: ColumnOptions = ColumnOptions

  def * : ColumnBase[T]
  final def column[C: TypeMapper](name: String, options: ColumnOption[C, ProfileType]*) = {

    val node = nodeDelegate match {
      case j: Join  => j.extractNode(tableName, forTableAlias = false)
      case delegate => delegate
    }
    new NamedColumn[C](node, name, options*)
  }

  def create_* : Iterable[NamedColumn[?]] = {
    def createTableError(msg: String) = Fail(
      s"Cannot use $msg in ${tableName}.* CREATE TABLE statement"
    )
    Node(*) match {
      case p: Projection[_] =>
        0 until p.productArity map (n => Node(p.productElement(n)) match {
          case c: NamedColumn[_] => c
          case c                 => createTableError(s"column $c")
        })
      case n: NamedColumn[_] => Iterable(n)
      case n                 => createTableError(s"$n")
    }
  }

  def foreignKey[P, PU, TT <: Table[?], U](name: String, sourceColumns: P, targetTable: TT)(
    targetColumns: TT => P,
    onUpdate: ForeignKeyAction = ForeignKeyAction.NoAction,
    onDelete: ForeignKeyAction = ForeignKeyAction.NoAction
  )(using unpackT: Unpack[TT, U], unpackP: Unpack[P, PU]): ForeignKeyQuery[TT, U] = {
    val targetUnpackable = Unpackable(
      targetTable.mapOp(Table.Alias.apply), unpackT
    )
    val fk = new ForeignKey(
      name, this, targetUnpackable, targetTable, unpackP,
      sourceColumns, targetColumns, onUpdate, onDelete
    )
    new ForeignKeyQuery(List(fk), targetUnpackable)
  }

  def primaryKey[TT](name: String, sourceColumns: TT)(using unpack: Unpack[TT, ?]): PrimaryKey =
    PrimaryKey(name, unpack.linearizer(sourceColumns).getLinearizedNodes)

  private def tableConstraints: Iterator[Constraint] =
    getClass().getMethods.iterator.filter( m =>
      m.getParameterTypes.length == 0 &&
      classOf[Constraint].isAssignableFrom(m.getReturnType)
    ).map { m =>
      m.setAccessible(true)
      m.invoke(this).asInstanceOf[Constraint]
    }

  final def foreignKeys: Iterable[ForeignKey[? <: Table[?], ?]] =
    tableConstraints.collect {
      case q: ForeignKeyQuery[_, _] => q.fks
    }.toIndexedSeq.flatten

  final def primaryKeys: Iterable[PrimaryKey] =
    tableConstraints.collect { case k: PrimaryKey => k }.toIndexedSeq

  def index[TT](name: String, on: TT, unique: Boolean = false)(using unpack: Unpack[TT, ?]) = new Index(
    name, this, unpack.linearizer(on).getLinearizedNodes, unique
  )

  def indexes: Iterable[Index] =
    getClass().getMethods.view.filter( m =>
      m.getParameterTypes.length == 0 && m.getReturnType == classOf[Index]
    ).map { m =>
      m.setAccessible(true)
      m.invoke(this).asInstanceOf[Index]
    }

  final def getLinearizedNodes = *.getLinearizedNodes

  final def getResult(profile: Profile, rs: PositionedResult) =
    *.getResult(profile, rs)

  final def updateResult(profile: Profile, rs: PositionedResult, value: T) =
    *.updateResult(profile, rs, value)

  final def setParameter(profile: Profile, ps: PositionedParameters, value: Option[T]) =
    *.setParameter(profile, ps, value)

  def ddl(using profile: ProfileType): DDL = profile.buildTableDDL(this)
}

object Table {
  def unapply[T](t: Table[T]) = Some(t.tableName)

  case class Alias(child: Node) extends UnaryNode {
    override def toString = s"Table.Alias $child"
    override def isNamedTable = true
  }
  case class Ref(table: Node, tableJoin: Option[Join])
  case class Name(alias: String, isFresh: Boolean)
}
