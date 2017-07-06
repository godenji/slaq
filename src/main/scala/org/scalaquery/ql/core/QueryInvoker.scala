package org.scalaquery.ql.core

import org.scalaquery.{
	UnitInvokerMixin, MutatingStatementInvoker, MutatingUnitInvoker
}
import org.scalaquery.ql.Query

final class QueryInvoker[P, R](val query: Query[_, R], val profile: Profile)
  extends MutatingStatementInvoker[Unit, R] 
		with UnitInvokerMixin[R] 
		with MutatingUnitInvoker[R] {

  override final protected val delegate = this
  final def invoker: this.type = this
}
