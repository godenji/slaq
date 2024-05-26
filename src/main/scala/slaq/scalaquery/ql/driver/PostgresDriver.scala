package slaq.ql.driver

import java.util.UUID
import slaq.ql._
import slaq.ql.core._
import slaq.util._
import slaq.session.{PositionedResult, PositionedParameters}

class PostgresDriver extends Profile { self =>

  type ImplicitT = ImplicitConversions[PostgresDriver]
  type TypeMapperDelegatesT = PostgresTypeMapperDelegates

  val Implicit = new ImplicitConversions[PostgresDriver] {
    given driverType: self.type = self
  }

  val typeMapperDelegates = new PostgresTypeMapperDelegates

  override def createQueryBuilder(query: Query[?, ?], nc: NamingContext) = new PostgresQueryBuilder(query, nc, None, this)
  override def buildTableDDL(table: Table[?]): DDL = new PostgresDDLBuilder(table, this).buildDDL
}

object PostgresDriver extends PostgresDriver

class PostgresTypeMapperDelegates extends TypeMapperDelegates {
  override val byteArrayTypeMapperDelegate = new TypeMapperDelegates.ByteArrayTypeMapperDelegate {
    override val sqlTypeName = "BYTEA"
  }
  override val uuidTypeMapperDelegate = new TypeMapperDelegates.UUIDTypeMapperDelegate {
    override def setValue(v: UUID, p: PositionedParameters) = p.setObject(v, sqlType)
    override def setOption(v: Option[UUID], p: PositionedParameters) = p.setObjectOption(v, sqlType)
    override def nextValue(r: PositionedResult) = r.nextObject().asInstanceOf[UUID]
    override def updateValue(v: UUID, r: PositionedResult) = r.updateObject(v)
    override def value2SQLLiteral(value: UUID) = "'" + value + "'"
  }

  override val byteTypeMapperDelegate = new ByteTypeMapperDelegate

  /* PostgreSQL does not have a TINYINT type, so we use SMALLINT instead. */
  class ByteTypeMapperDelegate extends TypeMapperDelegates.ByteTypeMapperDelegate {
    override def sqlTypeName = "SMALLINT"
  }
}

class PostgresQueryBuilder(_query: Query[?, ?], _nc: NamingContext, parent: Option[QueryBuilder], profile: PostgresDriver)
  extends QueryBuilder(_query, _nc, parent, profile) {

  override type Self = PostgresQueryBuilder
  override protected val concatOperator = Some("||")

  protected def createSubQueryBuilder(query: Query[?, ?], nc: NamingContext) =
    new PostgresQueryBuilder(query, nc, Some(this), profile)

  override protected def show(c: Node, b: SqlBuilder): Unit = c match {
    case Sequence.Nextval(seq) => b += s"nextval('${seq.name}')"
    case Sequence.Currval(seq) => b += s"currval('${seq.name}')"
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

  override protected def appendGroupClause(b: SqlBuilder): Unit =
    queryModifiers[Grouping] match {
      case Nil =>
      case xs =>
        b += " GROUP BY "
        b.sep(xs, ",")(x => expr(x.by, b, false))

        // existing groupBy columns
        val groupCols = xs.collect(_.by match {
          case nc: NamedColumn[_] => Some(nc.name)
          case _                  => None
        }).flatten

        //query.reified.dump("reif")
        ansiAppendGroupClause(query.reified, groupCols)(b)
    }

  /*
   * SQL standard requires that queries with a group by clause
   * must include non-aggregate columns from select list
   * where a functional dependency exists.
   *
   * In practice, the PK column of dependent table *not* explicitly
   * referenced in group by clause can be implicitly appended,
   * thus satisfying the standard while avoiding useless boilerplate
   * in queries.
   *
   * example sql standard breaking query:
   * 	for{o <- Order; u <- o.user; groupBy o.userID} yield(o,u)
   *
   * with implicit appending of dependent cols the following
   * standards compliant sql is generated:
   *
   * select o.*, u.*
   * from order o, user u
   * where o.userID = u.id
   * group by o.userID, u.id // u.id implicitly appended
   */
  private def ansiAppendGroupClause(
    node: Node, groupCols: List[String]
  )(b: SqlBuilder): Unit = {

    def matchColumn(n: NamedColumn[?]) = {
      if ( // append if is PK and not already exist in groupCols
      n.options.exists(_ == ColumnOption.PrimaryKey) &&
        !groupCols.exists(_ == n.name)) { b += ","; expr(n, b, false) }
    }
    def matchTable(n: Node) = n match {
      case Table.Alias(t) => ansiAppendGroupClause(t, groupCols)(b)
      case t: Table[_]    => ansiAppendGroupClause(t.*, groupCols)(b)
    }
    node match {
      case p: ProductNode =>
        p.nodeChildren.foreach { x =>
          x match {
            // aggregrate column, no need to append
            case _: SimpleFunction | ColumnOps.CountDistinct(_) =>
            case _: Table[_] | _: Table.Alias => matchTable(x)
            case nc: NamedColumn[_] => matchColumn(nc)
            case p: Projection[_] =>
              p.nodeChildren.foreach {
                case (nc: NamedColumn[_]) =>
                  matchColumn(nc)
              }
            case n =>
              //println(s"inner fallback $n")
              b += ","; expr(n, b, false)
          }
        }
      case _: Table[_] | _: Table.Alias => matchTable(node)
      case _ => //println(s"outer fallback $node")
    }
  }
}

class PostgresDDLBuilder(table: Table[?], profile: PostgresDriver) extends DDLBuilder(table, profile) {
  import profile.sqlUtils._

  protected class PostgresColumnDDLBuilder(column: NamedColumn[?]) extends ColumnDDLBuilder(column) {
    override def appendColumn(sb: StringBuilder): Unit = {
      sb append quote(column.name) append ' '
      if (autoIncrement) {
        sb append "SERIAL"
        autoIncrement = false
      }
      else sb append sqlType
      appendOptions(sb)
    }
  }

  override protected def createColumnDDLBuilder(c: NamedColumn[?]) = new PostgresColumnDDLBuilder(c)
}
