package org.scalaquery.ql.driver

import org.scalaquery.ql._
import org.scalaquery.ql.core._
import org.scalaquery.util._

class OracleDriver extends Profile { self =>

  type ImplicitT = ImplicitConversions[OracleDriver]
  type TypeMapperDelegatesT = TypeMapperDelegates

  val Implicit = new ImplicitConversions[OracleDriver] {
    implicit val scalaQueryDriver = self
  }

  val typeMapperDelegates = new TypeMapperDelegates {}

  override def createQueryBuilder(query: Query[_,_], nc: NamingContext) = new OracleQueryBuilder(query, nc, None, this)
}

object OracleDriver extends OracleDriver

class OracleQueryBuilder(_query: Query[_,_], _nc: NamingContext, parent: Option[QueryBuilder], profile: OracleDriver)
extends QueryBuilder(_query, _nc, parent, profile) {


  override type Self = OracleQueryBuilder
  override protected val scalarFrom = Some("DUAL")
  override protected val concatOperator = Some("||")

  protected def createSubQueryBuilder(query: Query[_,_], nc: NamingContext) =
    new OracleQueryBuilder(query, nc, Some(this), profile)

  override protected def innerBuildSelect(b: SQLBuilder, rename: Boolean) {
    query.typedModifiers[TakeDrop] match {
      case TakeDrop(Some(t), None, _) :: _ =>
        b += "SELECT * FROM (SELECT "
        expr(query.reified, b, rename, true)
        fromSlot = b.createSlot
        appendClauses(b)
        appendLimitValue(b += ") WHERE ROWNUM <= ",t)
      case TakeDrop(to, Some(d), _) :: _ =>
        b += "SELECT * FROM (SELECT t0.*, ROWNUM ROWNUM_O FROM (SELECT "
        expr(Node(query.reified), b, rename, true)
        b += ",ROWNUM ROWNUM_I"
        fromSlot = b.createSlot
        appendClauses(b)
        b += ") t0) WHERE ROWNUM_O"
        to match {
          case Some(t) =>
          	appendLimitValue(b+= " BETWEEN (1+",d)
          	appendLimitValue(b+= ") AND (",d)
          	appendLimitValue(b+= "+",t)
          	b+= ")"
          case None => appendLimitValue(b += ">",d)
        }
        b += " ORDER BY ROWNUM_I"
      case _ =>
        b += "SELECT "
        expr(query.reified, b, rename, true)
        fromSlot = b.createSlot
        appendClauses(b)
    }
  }

  override protected def appendLimitClause(b: SQLBuilder) = ()
}
