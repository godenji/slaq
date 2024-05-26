package slaq.ql.core

import slaq.{
  UnitInvokerMixin,
  MutatingStatementInvoker,
  MutatingUnitInvoker
}
import slaq.ql.Query

final class QueryInvoker[P, R](val query: Query[?, R], val profile: Profile)
  extends MutatingStatementInvoker[Unit, R]
  with UnitInvokerMixin[R]
  with MutatingUnitInvoker[R] {

  override final protected val delegate = this
  final def invoker: this.type = this
}
