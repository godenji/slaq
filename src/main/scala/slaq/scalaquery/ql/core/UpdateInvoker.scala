package slaq.ql.core

import slaq.ql.{Query, ColumnBase}
import slaq.session.{Session, PositionedParameters}
import slaq.util.NamingContext

final class UpdateInvoker[T](
  query: Query[_ <: ColumnBase[T], T], profile: Profile
) {

  final protected lazy val built =
    profile.buildUpdate(query, NamingContext())

  inline final protected def getStatement = built.sql
  def updateStatement = getStatement

  def update(value: T)(using session: Session): Int =
    session.withPreparedStatement(updateStatement) { st =>
      st.clearParameters
      val pp = new PositionedParameters(st)
      query.unpackable.value.setParameter(profile, pp, Some(value))
      built.setter(pp, null)
      st.executeUpdate()
    }

  def updateInvoker: this.type = this
}
