package org.scalaquery.ql.core

import org.scalaquery.MutatingStatementInvoker
import org.scalaquery.ql.Query
import scala.annotation.unchecked.{uncheckedVariance => uV}

final class QueryTemplate[P, +R](val query: Query[_, R], val profile: Profile)
  extends MutatingStatementInvoker[P, R @uV]
