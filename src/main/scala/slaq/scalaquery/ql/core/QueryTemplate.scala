package slaq.ql.core

import slaq.{AppliedInvoker, MutatingStatementInvoker, MutatingUnitInvoker}
import slaq.ql.Query
import slaq.ql.core.Profile
import slaq.util.{NamingContext, SqlBuilder}
import scala.annotation.unchecked.{uncheckedVariance => uV}

final class QueryTemplate[P, +R](val query: Query[P, R], val profile: Profile)
  extends MutatingStatementInvoker[P, R @uV] { self =>

  final private lazy val builtUpdate = profile.buildUpdate(query, NamingContext())
  final private lazy val builtDelete = profile.buildDelete(query, NamingContext())

  private type Apply = MutatingUnitInvoker[R @uV] & LockFreeMutatingInvoker[P, R @uV]

  override def apply(parameter: P): Apply =
    new AppliedInvoker[P, R]
      with MutatingUnitInvoker[R]
      with LockFreeMutatingInvoker[P, R] {

      protected val query = self.query
      protected val profile = self.profile
      protected def builtUpdate = self.builtUpdate
      protected def builtDelete = self.builtDelete

      protected val appliedParameter = parameter
      protected val delegate = self
    }
}
