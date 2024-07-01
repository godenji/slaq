package slaq.ql.core

import slaq.AppliedInvoker
import slaq.ql.{Query, ColumnBase}
import slaq.ql.core.Profile
import slaq.util.{NamingContext, SqlBuilder}
import slaq.session.{Session, PositionedParameters}
import scala.annotation.unchecked.{uncheckedVariance => uV}

/**
 * provides lock-free (i.e. without "SELECT ... FOR UPDATE") versions of update/delete
 * that would otherwise incur a row level lock via update/delete options
 * on MutatingUnitInvoker
 */
trait LockFreeMutatingInvoker[-P, R] { self: AppliedInvoker[P, R] =>
  protected def query: Query[?, R]
  protected def profile: Profile
  protected def builtUpdate: SqlBuilder.Result
  protected def builtDelete: SqlBuilder.Result

  def updateStatement = builtUpdate.sql
  def deleteStatement = builtDelete.sql

  /**
   * update target row(s) using value
   * note: lock-free; for row level locking semantics use `mutate` update option
   */
  def update(value: R)(using session: Session): Int =
    session.withPreparedStatement(builtUpdate.sql) { st =>
      st.clearParameters
      val pp = new PositionedParameters(st)
      val q = query.asInstanceOf[Query[? <: ColumnBase[R], R]]
      q.unpackable.value.setParameter(profile, pp, Some(value))
      builtUpdate.setter(pp, appliedParameter)
      st.executeUpdate()
    }

  /**
   * delete target row(s)
   * note: lock-free; for row level locking semantics use `mutate` delete option
   */
  def delete(using session: Session): Int =
    session.withPreparedStatement(builtDelete.sql) { st =>
      builtDelete.setter(new PositionedParameters(st), appliedParameter)
      st.executeUpdate()
    }
}
