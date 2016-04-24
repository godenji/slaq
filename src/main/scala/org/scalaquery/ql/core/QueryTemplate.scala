package org.scalaquery.ql.core

import java.sql.PreparedStatement
import org.scalaquery.MutatingStatementInvoker
import org.scalaquery.ql.Query
import org.scalaquery.session.{PositionedParameters, PositionedResult}
import org.scalaquery.util.{ValueLinearizer, NamingContext}
import scala.annotation.unchecked.{uncheckedVariance => uV}

final class QueryTemplate[P, +R](query: Query[_,R], profile: Profile) 
	extends MutatingStatementInvoker[P, R @uV] {

  final protected lazy val (built, lin) = 
  	profile.buildSelect(query, NamingContext())

	@inline final protected def getStatement = built.sql
  def selectStatement = getStatement

  final protected def setParam(param: P, st: PreparedStatement): 
  	Unit = built.setter(new PositionedParameters(st), param)

  final protected def extractValue(rs: PositionedResult): 
  	R = lin.asInstanceOf[ValueLinearizer[R]].getResult(profile, rs)

  final protected def updateRowValues(rs: PositionedResult, value: R @uV) = 
  	lin.asInstanceOf[ValueLinearizer[R]].updateResult(profile, rs, value)
}
