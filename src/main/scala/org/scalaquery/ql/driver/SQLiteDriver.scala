package org.scalaquery.ql.driver

import org.scalaquery.Fail
import org.scalaquery.ql._
import org.scalaquery.ql.core._
import org.scalaquery.util._
import java.sql.{Timestamp, Time, Date}

/**
 * ScalaQuery driver for SQLite.
 *
 * <p>This driver implements the Profile with the following
 * limitations:</p>
 * <ul>
 *   <li>Sequences are not supported because SQLite does not have them.</li>
 *   <li>Blobs are not supported by the SQLite JDBC driver (but binary data in
 *     the form of <code>Array[Byte]</code> is).</li>
 *   <li>SQLite does not allow mutation of result sets. All cursors are
 *     read-only.</li>
 *   <li><code>Functions.user</code> and <code>Functions.database</code> are
 *     not available in SQLite. ScalaQuery will return empty strings for
 *     both.</li>
 * </ul>
 */
class SQLiteDriver extends Profile { self =>

  type ImplicitT = ImplicitConversions[SQLiteDriver]
  type TypeMapperDelegatesT = TypeMapperDelegates

  val Implicit = new ImplicitConversions[SQLiteDriver] {
    implicit val driverType = self
  }

  val typeMapperDelegates = new SQLiteTypeMapperDelegates

  override def createQueryBuilder(query: Query[_, _], nc: NamingContext) = new SQLiteQueryBuilder(query, nc, None, this)
  override def buildTableDDL(table: Table[_]): DDL = new SQLiteDDLBuilder(table, this).buildDDL
}

object SQLiteDriver extends SQLiteDriver

class SQLiteTypeMapperDelegates extends TypeMapperDelegates {
  import SQLiteTypeMapperDelegates._
  override val booleanTypeMapperDelegate = new BooleanTypeMapperDelegate
  override val dateTypeMapperDelegate = new DateTypeMapperDelegate
  override val timeTypeMapperDelegate = new TimeTypeMapperDelegate
  override val timestampTypeMapperDelegate = new TimestampTypeMapperDelegate
  override val uuidTypeMapperDelegate = new UUIDTypeMapperDelegate
}

object SQLiteTypeMapperDelegates {
  /* SQLite does not have a proper BOOLEAN type. The suggested workaround is
   * INTEGER with constants 1 and 0 for TRUE and FALSE. */
  class BooleanTypeMapperDelegate extends TypeMapperDelegates.BooleanTypeMapperDelegate {
    override def sqlTypeName = "INTEGER"
    override def value2SQLLiteral(value: Boolean) = if (value) "1" else "0"
  }
  /* The SQLite JDBC driver does not support the JDBC escape syntax for
   * date/time/timestamp literals. SQLite expects these values as milliseconds
   * since epoch. */
  class DateTypeMapperDelegate extends TypeMapperDelegates.DateTypeMapperDelegate {
    override def value2SQLLiteral(value: Date) = value.getTime.toString
  }
  class TimeTypeMapperDelegate extends TypeMapperDelegates.TimeTypeMapperDelegate {
    override def value2SQLLiteral(value: Time) = value.getTime.toString
  }
  class TimestampTypeMapperDelegate extends TypeMapperDelegates.TimestampTypeMapperDelegate {
    override def value2SQLLiteral(value: Timestamp) = value.getTime.toString
  }
  class UUIDTypeMapperDelegate extends TypeMapperDelegates.UUIDTypeMapperDelegate {
    override def sqlType = java.sql.Types.BLOB
  }
}

class SQLiteDDLBuilder(table: Table[_], profile: SQLiteDriver)
  extends DDLBuilder(table, profile) {

  protected class SQLiteColumnDDLBuilder(column: NamedColumn[_]) extends ColumnDDLBuilder(column) {
    override protected def appendOptions(sb: StringBuilder) {
      if (defaultLiteral ne null) sb append " DEFAULT " append defaultLiteral
      if (autoIncrement) sb append " PRIMARY KEY AUTOINCREMENT"
      else if (notNull) sb append " NOT NULL"
      else if (primaryKey) sb append " PRIMARY KEY"
    }
  }

  override protected def createColumnDDLBuilder(c: NamedColumn[_]) = new SQLiteColumnDDLBuilder(c)

  override def buildDDL: DDL = {
    val b = new StringBuilder append "CREATE TABLE " append table.tableName append " ("
    var first = true
    for (n <- table.create_*) {
      if (first) first = false
      else b append ','
      createColumnDDLBuilder(n).appendColumn(b)
    }
    var prevPK: String = null
    for (pk <- table.primaryKeys) {
      if (prevPK eq null) prevPK = pk.name
      else Fail("Table " + table.tableName + " defines multiple primary keys " + prevPK + " and " + pk.name)
      b append ','
      addPrimaryKey(pk, b)
    }
    for (fk <- table.foreignKeys) {
      b append ','
      addForeignKey(fk, b)
    }
    b append ')'
    new DDL {
      val createPhase1 = Iterable(b.toString)
      val createPhase2 = Iterable()
      val dropPhase1 = Nil
      val dropPhase2 = Iterable("DROP TABLE " + table.tableName)
    }
  }
}

class SQLiteQueryBuilder(_query: Query[_, _], _nc: NamingContext, parent: Option[QueryBuilder], profile: SQLiteDriver)
  extends QueryBuilder(_query, _nc, parent, profile) {

  override type Self = SQLiteQueryBuilder
  override protected val concatOperator = Some("||")

  protected def createSubQueryBuilder(query: Query[_, _], nc: NamingContext) =
    new SQLiteQueryBuilder(query, nc, Some(this), profile)

  override protected def appendOrdering(o: Ordering, b: SqlBuilder) {
    val desc = o.isInstanceOf[Ordering.Desc]
    if (o.nullOrdering == Ordering.NullsLast && !desc) {
      b += '('
      expr(o.by, b)
      b += ") is null,"
    } else if (o.nullOrdering == Ordering.NullsFirst && desc) {
      b += '('
      expr(o.by, b)
      b += ") is null desc,"
    }
    expr(o.by, b)
    if (desc) b += " desc"
  }

  override protected def appendLimitClause(b: SqlBuilder) = queryModifiers[TakeDrop].lastOption.foreach {
    case TakeDrop(Some(t), Some(d), compareNode) =>
      val compFn = maybeLimitNode(t, d, compareNode, _: Boolean)
      appendLimitValue(b += " LIMIT ", d, compFn(true))
      appendLimitValue(b += ',', t, compFn(false))

    case TakeDrop(Some(t), None, _) => appendLimitValue(b += " LIMIT ", t)
    case TakeDrop(None, Some(d), _) =>
      appendLimitValue(b += " LIMIT ", d); b += ",-1"
    case _                          =>
  }

  override protected def show(c: Node, b: SqlBuilder): Unit = c match {
    case fk: ForeignKey[_, _] =>
      val cols = fk.linearizedSourceColumns.zip(fk.linearizedTargetColumns)
      b += '('
      b.sep(cols, " AND ") { case (l, r) => expr(l, b); b += " = "; expr(r, b) }
      b += ')'
    case StdFunction("exists", q: Query[_, _]) =>
      // SQLite rejects double parens around sub-expression
      b += "exists"; show(q, b)
    case EscFunction("ucase", ch, _)   =>
      b += "upper("; expr(ch, b); b += ')'
    case EscFunction("lcase", ch, _)   =>
      b += "lower("; expr(ch, b); b += ')'
    case EscFunction("mod", l, r)      =>
      b += '('; expr(l, b); b += '%'; expr(r, b); b += ')'
    case EscFunction("ceiling", ch, _) =>
      b += "round("; expr(ch, b); b += "+0.5)"
    case EscFunction("floor", ch, _)   =>
      b += "round("; expr(ch, b); b += "-0.5)"
    case EscFunction("user", _, _)     => b += "''"
    case EscFunction("database", _, _) => b += "''"
    case s: SimpleFunction if s.scalar && s.name != "concat" =>
      /* The SQLite JDBC driver does not support ODBC {fn ...} escapes, so we try
       * unescaped function calls by default */
      b += s.name += '('
      b.sep(s.nodeChildren, ",")(expr(_, b))
      b += ')'
    case _ => super.show(c, b)
  }
}
