package org.scalaquery.ql.core

import org.scalaquery.ql.{Query, ColumnBase}
import org.scalaquery.session.{Session, PositionedParameters}
import org.scalaquery.util.NamingContext

final class UpdateInvoker[T](
  query: Query[_ <: ColumnBase[T], T], profile: Profile
) {

  final protected lazy val built =
    profile.buildUpdate(query, NamingContext())

  @inline final protected def getStatement = built.sql
  def updateStatement = getStatement

  def update(value: T)(implicit session: Session): Int =
    session.withPreparedStatement(updateStatement) { st =>
      st.clearParameters
      val pp = new PositionedParameters(st)
      query.unpackable.value.setParameter(profile, pp, Some(value))
      built.setter(pp, null)
      st.executeUpdate
    }

  def updateInvoker: this.type = this
}
