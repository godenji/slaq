package slaq.ql.core

import slaq.MutatingStatementInvoker
import slaq.ql.Query
import scala.annotation.unchecked.{uncheckedVariance => uV}

final class QueryTemplate[P, +R](val query: Query[P, R], val profile: Profile)
  extends MutatingStatementInvoker[P, R @uV]
