package org.scalaquery.ql.core

import java.sql.PreparedStatement
import org.scalaquery.MutatingStatementInvoker
import org.scalaquery.ql.Query
import org.scalaquery.session.{PositionedParameters, PositionedResult}
import org.scalaquery.util.{ValueLinearizer, NamingContext}

final class QueryTemplate[P, R](query: Query[_,R], profile: Profile) 
	extends MutatingStatementInvoker[P, R] {

  protected lazy val (built, lin) = 
  	profile.buildSelect(query, NamingContext())

  @inline final def selectStatement = getStatement
  def pretty = 
  	getStatement.replaceAll("`", "").replaceAll("\"", "").split(",").mkString(", ").
  	replaceAll("(FROM|INNER|LEFT|RIGHT|FULL|WHERE|GROUP BY|ORDER BY|LIMIT)", "\n$1")

  protected def getStatement = built.sql

  protected def setParam(param: P, st: PreparedStatement): 
  	Unit = built.setter(new PositionedParameters(st), param)

  protected def extractValue(rs: PositionedResult): 
  	R = lin.asInstanceOf[ValueLinearizer[R]].getResult(profile, rs)

  protected def updateRowValues(rs: PositionedResult, value: R) = 
  	lin.asInstanceOf[ValueLinearizer[R]].updateResult(profile, rs, value)
}
