package slaq.ql.core

import slaq.Fail
import slaq.ql._
import slaq.util.Node
import scala.collection.immutable.IndexedSeq

class DDLBuilder(val table: Table[?], val profile: Profile) {
  import profile.sqlUtils._

  protected class ColumnDDLBuilder(protected val column: NamedColumn[?]) {

    protected val tmDelegate = column.typeMapper(profile)
    protected var sqlType: String = null
    protected var notNull = !tmDelegate.nullable
    protected var primaryKey = false
    protected var autoIncrement = false
    protected var defaultLiteral: String = null
    init()

    protected def init(): Unit = {
      for (o <- column.options) handleColumnOption(o)
      if (sqlType eq null) sqlType = mapTypeName(tmDelegate)
    }

    protected def handleColumnOption(o: ColumnOption[?, ?]): Unit = o match {
      case ColumnOption.DBType(s)  => sqlType = s
      case ColumnOption.NotNull    => notNull = true
      case ColumnOption.Nullable   => notNull = false
      case ColumnOption.PrimaryKey => primaryKey = true
      case ColumnOption.Default(v) => defaultLiteral =
        column.asInstanceOf[NamedColumn[Any]].typeMapper(profile).value2SQLLiteral(v)
      case ColumnOption.AutoInc => autoIncrement = true
    }

    def appendColumn(sb: StringBuilder): Unit = {
      sb append quote(column.name) append ' '
      sb append sqlType
      appendOptions(sb)
    }

    protected def appendOptions(sb: StringBuilder): Unit = {
      if (defaultLiteral ne null) sb append " DEFAULT " append defaultLiteral
      if (notNull) sb append " NOT NULL"
      if (autoIncrement) sb append " AUTO_INCREMENT"
      if (primaryKey) sb append " PRIMARY KEY"
    }
  }

  protected def createColumnDDLBuilder(c: NamedColumn[?]) = new ColumnDDLBuilder(c)

  def buildDDL: DDL = {
    val createTable = {
      val b = new StringBuilder append "CREATE TABLE " append quote(table.tableName) append " ("
      var first = true
      for (n <- table.create_*) {
        if (first) first = false
        else b append ","
        createColumnDDLBuilder(n).appendColumn(b)
      }
      b append ")"
      b.toString
    }
    val createIndexes = table.indexes.map(createIndex)
    val foreignKeys = table.foreignKeys
    val primaryKeys = table.primaryKeys
    if (primaryKeys.size > 1)
      Fail("Table " + table.tableName + " defines multiple primary keys")
    new DDL {
      val createPhase1 = Iterable(createTable) ++ primaryKeys.map(createPrimaryKey) ++ createIndexes
      val createPhase2 = foreignKeys.map(createForeignKey)
      val dropPhase1 = foreignKeys.map(dropForeignKey)
      val dropPhase2 = primaryKeys.map(dropPrimaryKey) ++ Iterable("DROP TABLE " + quote(table.tableName))
    }
  }

  protected def createIndex(idx: Index) = {
    val b = new StringBuilder append "CREATE "
    if (idx.unique) b append "UNIQUE "
    b append "INDEX " append quote(idx.name) append " ON " append quote(table.tableName) append "("
    addIndexColumnList(idx.on, b, idx.table.tableName)
    b append ")"
    b.toString
  }

  protected def createForeignKey(fk: ForeignKey[? <: Table[?], ?]) = {
    val sb = new StringBuilder append "ALTER TABLE " append quote(table.tableName) append " ADD "
    addForeignKey(fk, sb)
    sb.toString
  }

  protected def addForeignKey(fk: ForeignKey[? <: Table[?], ?], sb: StringBuilder): Unit = {
    sb append "CONSTRAINT " append quote(fk.name) append " FOREIGN KEY("
    addForeignKeyColumnList(fk.linearizedSourceColumns, sb, table.tableName)
    sb append ") REFERENCES " append quote(fk.targetTable.tableName) append "("
    addForeignKeyColumnList(fk.linearizedTargetColumnsForOriginalTargetTable, sb, fk.targetTable.tableName)
    sb append ") ON UPDATE " append fk.onUpdate.action
    sb append " ON DELETE " append fk.onDelete.action
  }

  protected def createPrimaryKey(pk: PrimaryKey) = {
    val sb = new StringBuilder append "ALTER TABLE " append quote(table.tableName) append " ADD "
    addPrimaryKey(pk, sb)
    sb.toString
  }

  protected def addPrimaryKey(pk: PrimaryKey, sb: StringBuilder): Unit = {
    sb append "CONSTRAINT " append quote(pk.name) append " PRIMARY KEY("
    addPrimaryKeyColumnList(pk.columns, sb, table.tableName)
    sb append ")"
  }

  protected def dropForeignKey(fk: ForeignKey[? <: Table[?], ?]) = {
    "ALTER TABLE " + quote(table.tableName) + " DROP CONSTRAINT " + quote(fk.name)
  }

  protected def dropPrimaryKey(pk: PrimaryKey) = {
    "ALTER TABLE " + quote(table.tableName) + " DROP CONSTRAINT " + quote(pk.name)
  }

  protected def addIndexColumnList(columns: IndexedSeq[Node], sb: StringBuilder, requiredTableName: String) =
    addColumnList(columns, sb, requiredTableName, "index")

  protected def addForeignKeyColumnList(columns: IndexedSeq[Node], sb: StringBuilder, requiredTableName: String) =
    addColumnList(columns, sb, requiredTableName, "foreign key constraint")

  protected def addPrimaryKeyColumnList(columns: IndexedSeq[Node], sb: StringBuilder, requiredTableName: String) =
    addColumnList(columns, sb, requiredTableName, "foreign key constraint")

  protected def addColumnList(columns: IndexedSeq[Node], sb: StringBuilder, requiredTableName: String, typeInfo: String) = {
    var first = true
    for (c <- columns) c match {
      case n: NamedColumn[_] =>
        if (first) first = false
        else sb append ","
        sb append quote(n.name)
        if (requiredTableName != n.table.asInstanceOf[Table[?]].tableName)
          Fail("All columns in " + typeInfo + " must belong to table " + requiredTableName)
      case _ => Fail("Cannot use column " + c +
        " in " + typeInfo + " (only named columns are allowed)")
    }
  }
}
