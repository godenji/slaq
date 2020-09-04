package slaq.ql.driver

import slaq.ql._
import slaq.ql.core._
import slaq.util._

class H2Driver extends Profile { self =>

  type ImplicitT = ImplicitConversions[H2Driver]
  type TypeMapperDelegatesT = TypeMapperDelegates

  val Implicit = new ImplicitConversions[H2Driver] {
    implicit val driverType = self
  }

  val typeMapperDelegates = new TypeMapperDelegates {}
  override val sqlUtils = new H2SQLUtils

  override def createQueryBuilder(query: Query[_, _], nc: NamingContext) = new H2QueryBuilder(query, nc, None, this)
}

object H2Driver extends H2Driver

class H2QueryBuilder(_query: Query[_, _], _nc: NamingContext, parent: Option[QueryBuilder], profile: H2Driver)
  extends QueryBuilder(_query, _nc, parent, profile) {

  override type Self = H2QueryBuilder
  override protected val concatOperator = Some("||")

  protected def createSubQueryBuilder(query: Query[_, _], nc: NamingContext) =
    new H2QueryBuilder(query, nc, Some(this), profile)

  override protected def show(c: Node, b: SqlBuilder): Unit = c match {
    case Sequence.Nextval(seq) => b += s"nextval(schema(), '${seq.name}')"
    case Sequence.Currval(seq) => b += s"currval(schema(), '${seq.name}')"
    case _                     => super.show(c, b)
  }

  override protected def appendLimitClause(b: SqlBuilder) = queryModifiers[TakeDrop].lastOption.foreach {
    case TakeDrop(Some(t), Some(d), compareNode) =>
      val compFn = maybeLimitNode(t, d, compareNode, _: Boolean)
      appendLimitValue(b += " LIMIT ", t, compFn(false))
      appendLimitValue(b += " OFFSET ", d, compFn(true))
    /*
     * 'LIMIT 0' no longer works as of H2 1.4.185; need to limit/offset same dropN
     */
    case TakeDrop(None, Some(d), compareNode) =>
      val compFn = maybeLimitNode(ConstColumn(0), d, compareNode, _: Boolean)
      appendLimitValue(b += " LIMIT ", d, compFn(false)) // pretend drop is a take
      appendLimitValue(b += " OFFSET ", d, compFn(true))

    case TakeDrop(Some(t), None, _) => appendLimitValue(b += " LIMIT ", t)
    case _ =>
  }
}

class H2SQLUtils extends SQLUtils {
  override def mapTypeName(tmd: TypeMapperDelegate[_]): String = tmd.sqlType match {
    case java.sql.Types.VARCHAR => "VARCHAR"
    case _ => super.mapTypeName(tmd)
  }
}
