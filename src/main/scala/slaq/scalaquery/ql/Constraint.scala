package slaq.ql

import slaq.util.{Node, BinaryNode}
import scala.collection.immutable.IndexedSeq

sealed trait Constraint

case class PrimaryKey(name: String, columns: IndexedSeq[Node]) extends Constraint

enum ForeignKeyAction(val action: String):
  case Cascade extends ForeignKeyAction("CASCADE")
  case Restrict extends ForeignKeyAction("RESTRICT")
  case NoAction extends ForeignKeyAction("NO ACTION")
  case SetNull extends ForeignKeyAction("SET NULL")
  case SetDefault extends ForeignKeyAction("SET DEFAULT")

class ForeignKey[TT <: Table[?], P](
  val name: String,
  val sourceTable: Node,
  val targetTableUnpackable: Unpackable[TT, ?],
  originalTargetTable: TT,
  unpackp: Unpack[P, ?],
  originalSourceColumns: P,
  originalTargetColumns: TT => P,
  val onUpdate: ForeignKeyAction,
  val onDelete: ForeignKeyAction
) extends OperatorColumn[Boolean] with BinaryNode {

  val targetTable = targetTableUnpackable.value
  val left = Node(unpackp.reify(originalSourceColumns))
  val right = Node(unpackp.reify(originalTargetColumns(targetTable)))

  override def toString = s"ForeignKey $name"

  def linearizedSourceColumns =
    unpackp.linearizer(originalSourceColumns).getLinearizedNodes

  def linearizedTargetColumns =
    unpackp.linearizer(originalTargetColumns(targetTable)).getLinearizedNodes

  def linearizedTargetColumnsForOriginalTargetTable =
    unpackp.linearizer(
      originalTargetColumns(originalTargetTable)
    ).getLinearizedNodes

  def withTargetTableUnpackable(targetTableUnpackable: Unpackable[TT, ?]) =
    new ForeignKey[TT, P](
      name, sourceTable, targetTableUnpackable, originalTargetTable,
      unpackp, originalSourceColumns, originalTargetColumns, onUpdate, onDelete
    )
}

class ForeignKeyQuery[TT <: Table[?], U](
  val fks: List[ForeignKey[TT, ?]],
  override val unpackable: Unpackable[TT, U]
)
  extends QueryWrap[TT, U](unpackable, fks, Nil) with Constraint {

  override def toString = "ForeignKeyQuery"
  /**
   * chain foreign key constraints that point to the same table
   */
  def &(other: ForeignKeyQuery[TT, U]) = {
    val tt = fks.head.targetTableUnpackable
    new ForeignKeyQuery(fks ++ other.fks.map(
      _.withTargetTableUnpackable(tt)
    ), unpackable)
  }
}
