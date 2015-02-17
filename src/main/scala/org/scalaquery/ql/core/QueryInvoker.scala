package org.scalaquery.ql.core

import java.sql.PreparedStatement
import org.scalaquery.{
	UnitInvokerMixin, MutatingStatementInvoker, MutatingUnitInvoker
}
import org.scalaquery.session.{PositionedParameters, PositionedResult}
import org.scalaquery.ql.{Query, ColumnBase}
import org.scalaquery.util.{ValueLinearizer, NamingContext}

class QueryInvoker[Q, R](q: Query[Q, R], profile: Profile)
  extends MutatingStatementInvoker[Unit, R] 
	with UnitInvokerMixin[R] with MutatingUnitInvoker[R] {

  override protected val delegate = this

  protected lazy val (built, lin) = 
  	profile.buildSelectStatement(q, NamingContext())

  @inline final def selectStatement = getStatement
  def pretty = 
  	getStatement.replaceAll("`", "").replaceAll("\"", "").split(",").mkString(", ").
  	replaceAll("(FROM|INNER|LEFT|RIGHT|FULL|WHERE|GROUP BY|ORDER BY|LIMIT)", "\n$1")

  protected def getStatement = built.sql

  protected def setParam(param: Unit, st: PreparedStatement): Unit = 
  	built.setter(new PositionedParameters(st), null)

  protected def extractValue(rs: PositionedResult): R = 
  	lin.asInstanceOf[ValueLinearizer[R]].getResult(profile, rs)

  protected def updateRowValues(rs: PositionedResult, value: R) = 
  	lin.asInstanceOf[ValueLinearizer[R]].updateResult(profile, rs, value)

  def invoker: this.type = this
}
