package org.scalaquery.ql.driver

import org.scalaquery.ql._
import org.scalaquery.ql.core._
import org.scalaquery.util._

class H2Driver extends Profile { self =>

  type ImplicitT = ImplicitConversions[H2Driver]
  type TypeMapperDelegatesT = TypeMapperDelegates

  val Implicit = new ImplicitConversions[H2Driver] {
    implicit val scalaQueryDriver = self
  }

  val typeMapperDelegates = new TypeMapperDelegates {}
  override val sqlUtils = new H2SQLUtils

  override def createQueryBuilder(query: Query[_, _], nc: NamingContext) = new H2QueryBuilder(query, nc, None, this)
}

object H2Driver extends H2Driver

class H2QueryBuilder(_query: Query[_, _], _nc: NamingContext, parent: Option[QueryBuilder], profile: H2Driver)
	extends QueryBuilder(_query, _nc, parent, profile) {


  override type Self = H2QueryBuilder
  override protected val mayLimit0 = false
  override protected val concatOperator = Some("||")

  protected def createSubQueryBuilder(query: Query[_, _], nc: NamingContext) =
    new H2QueryBuilder(query, nc, Some(this), profile)

  override protected def innerExpr(c: Node, b: SQLBuilder): Unit = c match {
    case Sequence.Nextval(seq) => b += s"nextval(schema(), '${seq.name}')"
    case Sequence.Currval(seq) => b += s"currval(schema(), '${seq.name}')"
    case _ => super.innerExpr(c, b)
  }

  override protected def appendLimitClause(b: SQLBuilder) = query.typedModifiers[TakeDrop].lastOption.foreach {
    case TakeDrop(Some(ConstColumn(0)),_,_) => () // handled in innerBuildSelect
    case TakeDrop(Some(t), Some(d), compareNode) => 
    	appendColumnValue(b+=" LIMIT ", t, compareNode); appendColumnValue(b+=" OFFSET ",d)
    	
    case TakeDrop(Some(t), None, _) => appendColumnValue(b+=" LIMIT ",t)
    case TakeDrop(None, Some(d), _) => appendColumnValue(b+=" LIMIT 0 OFFSET ",d)
    case _ =>
  }
}

class H2SQLUtils extends SQLUtils {
  override def mapTypeName(tmd: TypeMapperDelegate[_]): String = tmd.sqlType match {
    case java.sql.Types.VARCHAR => "VARCHAR"
    case _ => super.mapTypeName(tmd)
  }
}
