package org.scalaquery.ql.core

import java.sql.PreparedStatement
import org.scalaquery.{
	UnitInvokerMixin, MutatingStatementInvoker, MutatingUnitInvoker
}
import org.scalaquery.session.{PositionedParameters, PositionedResult}
import org.scalaquery.ql.Query
import org.scalaquery.util.{ValueLinearizer, NamingContext}

final class QueryInvoker[Q, R](q: Query[Q, R], profile: Profile)
  extends MutatingStatementInvoker[Unit, R] 
	with UnitInvokerMixin[R] with MutatingUnitInvoker[R] {

  override final protected val delegate = this

  final protected lazy val (built, lin) = 
  	profile.buildSelect(q, NamingContext())

	@inline final protected def getStatement = built.sql
  def selectStatement = getStatement

  final protected def setParam(param: Unit, st: PreparedStatement): Unit = 
  	built.setter(new PositionedParameters(st), null)

  final protected def extractValue(rs: PositionedResult): R = 
  	lin.asInstanceOf[ValueLinearizer[R]].getResult(profile, rs)

  final protected def updateRowValues(rs: PositionedResult, value: R) = 
  	lin.asInstanceOf[ValueLinearizer[R]].updateResult(profile, rs, value)

  final def invoker: this.type = this
}
