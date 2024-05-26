package slaq.ql.driver

import java.sql.Types
import slaq.Fail
import slaq.ql._
import slaq.ql.core._
import slaq.util._

/**
 * ScalaQuery driver for <a href="http://www.hsqldb.org/">HyperSQL</a>
 * (starting with version 2.0).
 *
 * <p>This driver implements the Profile with the following
 * limitations:</p>
 * <ul>
 *   <li><code>Sequence.curr</code> to get the current value of a sequence is
 *     not supported by Hsqldb. Trying to generate SQL code which uses this
 *     feature throws a Fail.</li>
 * </ul>
 *
 * @author szeiger
 */
class HsqldbDriver extends Profile { self =>

  type ImplicitT = ImplicitConversions[HsqldbDriver]
  type TypeMapperDelegatesT = HsqldbTypeMapperDelegates

  val Implicit = new ImplicitConversions[HsqldbDriver] {
    given driverType: self.type = self
  }

  val typeMapperDelegates = new HsqldbTypeMapperDelegates

  override def createQueryBuilder(query: Query[?, ?], nc: NamingContext) = new HsqldbQueryBuilder(query, nc, None, this)
  override def buildTableDDL(table: Table[?]): DDL = new HsqldbDDLBuilder(table, this).buildDDL
  override def buildSequenceDDL(seq: Sequence[?]): DDL = new HsqldbSequenceDDLBuilder(seq, this).buildDDL
}

object HsqldbDriver extends HsqldbDriver

class HsqldbTypeMapperDelegates extends TypeMapperDelegates {
  override val byteArrayTypeMapperDelegate = new TypeMapperDelegates.ByteArrayTypeMapperDelegate {
    override val sqlTypeName = "LONGVARBINARY"
  }
  override val uuidTypeMapperDelegate = new TypeMapperDelegates.UUIDTypeMapperDelegate {
    override def sqlType = java.sql.Types.BINARY
    override def sqlTypeName = "BINARY(16)"
  }
}

class HsqldbDDLBuilder(table: Table[?], profile: HsqldbDriver) extends DDLBuilder(table, profile) {
  import profile.sqlUtils._

  protected class HsqldbColumnDDLBuilder(column: NamedColumn[?]) extends ColumnDDLBuilder(column) {
    override protected def appendOptions(sb: StringBuilder): Unit = {
      if (defaultLiteral ne null) sb append " DEFAULT " append defaultLiteral
      if (autoIncrement) sb append " GENERATED BY DEFAULT AS IDENTITY(START WITH 1)"
      if (notNull) sb append " NOT NULL"
      if (primaryKey) sb append " PRIMARY KEY"
    }
  }

  override protected def createColumnDDLBuilder(c: NamedColumn[?]) = new HsqldbColumnDDLBuilder(c)

  override protected def createIndex(idx: Index) = {
    if (idx.unique) {
      /* Create a UNIQUE CONSTRAINT (with an automatically generated backing
       * index) because Hsqldb does not allow a FOREIGN KEY CONSTRAINT to
       * reference columns which have a UNIQUE INDEX but not a nominal UNIQUE
       * CONSTRAINT. */
      val sb = new StringBuilder append "ALTER TABLE " append quote(table.tableName) append " ADD "
      sb append "CONSTRAINT " append quote(idx.name) append " UNIQUE("
      addIndexColumnList(idx.on, sb, idx.table.tableName)
      sb append ')'
      sb.toString
    }
    else super.createIndex(idx)
  }
}

class HsqldbQueryBuilder(_query: Query[?, ?], _nc: NamingContext, parent: Option[QueryBuilder], profile: HsqldbDriver)
  extends QueryBuilder(_query, _nc, parent, profile) {

  import profile.sqlUtils._

  override type Self = HsqldbQueryBuilder
  override protected val scalarFrom = Some("(VALUES (0))")
  override protected val concatOperator = Some("||")

  protected def createSubQueryBuilder(query: Query[?, ?], nc: NamingContext) =
    new HsqldbQueryBuilder(query, nc, Some(this), profile)

  override protected def show(c: Node, b: SqlBuilder): Unit = c match {

    case c @ ConstColumn(v: String) if v ne null =>
      /* Hsqldb treats string literals as type CHARACTER and pads them with
       * spaces in some expressions, so we cast all string literals to
       * VARCHAR. The length is only 16M instead of 2^31-1 in order to leave
       * enough room for concatenating strings (which extends the size even if
       * it is not needed). */
      if (c.typeMapper(profile).sqlType == Types.CHAR) super.show(c, b)
      else {
        b += "cast("
        super.show(c, b)
        b += " as varchar(16777216))"
      }

    /* Hsqldb uses the SQL:2008 syntax for NEXTVAL */
    case Sequence.Nextval(seq) => b += s"(next value for ${quote(seq.name)})"
    case Sequence.Currval(_) => Fail("Hsqldb does not support CURRVAL")
    case _                     => super.show(c, b)
  }

  override protected def appendLimitClause(b: SqlBuilder) = queryModifiers[TakeDrop].lastOption.foreach {
    case TakeDrop(Some(t), Some(d), compareNode) =>
      val compFn = maybeLimitNode(t, d, compareNode, _: Boolean)
      appendLimitValue(b += " LIMIT ", t, compFn(false))
      appendLimitValue(b += " OFFSET ", d, compFn(true))

    case TakeDrop(Some(t), None, _) => appendLimitValue(b += " LIMIT ", t)
    case TakeDrop(None, Some(d), _) => appendLimitValue(b += " OFFSET ", d)
    case _ =>
  }
}

class HsqldbSequenceDDLBuilder[T](seq: Sequence[T], profile: HsqldbDriver) extends SequenceDDLBuilder(seq, profile) {
  import profile.sqlUtils._

  override def buildDDL: DDL = {
    import seq.integral._
    val increment = seq._increment.getOrElse(one)
    val desc = increment < zero
    val start: String = seq._start.map(_.toString).getOrElse(if (desc) "-1" else "1")
    val b = new StringBuilder append "CREATE SEQUENCE " append quote(seq.name)
    seq._increment.foreach { b append " INCREMENT BY " append _ }
    seq._minValue.foreach { b append " MINVALUE " append _ }
    seq._maxValue.foreach { b append " MAXVALUE " append _ }
    /* The START value in Hsqldb defaults to 0 instead of the more
     * conventional 1/-1 so we rewrite it to make 1/-1 the default. */
    if (start != "0") b append " START WITH " append start
    if (seq._cycle) b append " CYCLE"
    new DDL {
      val createPhase1 = Iterable(b.toString)
      val createPhase2 = Nil
      val dropPhase1 = Nil
      val dropPhase2 = Iterable("DROP SEQUENCE " + quote(seq.name))
    }
  }
}
