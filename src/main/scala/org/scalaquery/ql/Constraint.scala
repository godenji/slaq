package org.scalaquery.ql

import org.scalaquery.util.{Node, BinaryNode}

sealed trait Constraint

case class PrimaryKey(name: String, columns: IndexedSeq[Node]) extends Constraint

sealed abstract class ForeignKeyAction(val action: String)
object ForeignKeyAction {
  case object Cascade extends ForeignKeyAction("CASCADE")
  case object Restrict extends ForeignKeyAction("RESTRICT")
  case object NoAction extends ForeignKeyAction("NO ACTION")
  case object SetNull extends ForeignKeyAction("SET NULL")
  case object SetDefault extends ForeignKeyAction("SET DEFAULT")
}

class ForeignKey[TT <: Table[_], P](
	val name: String, 
	val sourceTable: Node,
  val targetTableUnpackable: Unpackable[TT, _], 
  originalTargetTable: TT,
  unpackp: Unpack[P, _],
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
  	
  def withTargetTableUnpackable(targetTableUnpackable: Unpackable[TT, _]) =
    new ForeignKey[TT, P](
    	name, sourceTable, targetTableUnpackable, originalTargetTable,
      unpackp, originalSourceColumns, originalTargetColumns, onUpdate, onDelete
    )
}

class ForeignKeyQuery[TT <: Table[_], U](
	val fks: List[ForeignKey[TT, _]], 
	override val unpackable: Unpackable[TT, U]
)
extends QueryWrap[TT, U](unpackable, fks, Nil) with Constraint {
	
  override def toString = "ForeignKeyQuery"
  /**
   * chain foreign key constraints that point to the same table
   */
  def & (other: ForeignKeyQuery[TT, U]) = {
    val tt = fks.head.targetTableUnpackable
    new ForeignKeyQuery(fks ++ other.fks.map(
    	_.withTargetTableUnpackable(tt)
    ), unpackable)
  }
}
