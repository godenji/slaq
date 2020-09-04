package slaq.session

import java.sql.ResultSet

sealed abstract class ResultSetConcurrency(val intValue: Int) { self =>
  def apply[T](base: Session)(f: Session => T): T = f(base.forParameters(concurrency = self))
  def apply[T](f: => T)(implicit s: Session): T = apply(s)(_ => f)
  def withDefault(r: ResultSetConcurrency) = this
}

object ResultSetConcurrency {
  case object Auto extends ResultSetConcurrency(ResultSet.CONCUR_READ_ONLY) {
    override def withDefault(r: ResultSetConcurrency) = r
  }
  case object ReadOnly extends ResultSetConcurrency(ResultSet.CONCUR_READ_ONLY)
  case object Updatable extends ResultSetConcurrency(ResultSet.CONCUR_UPDATABLE)
}
