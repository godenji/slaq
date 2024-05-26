package slaq.ql.core

import slaq.ql.{Query, Table}
import slaq.session.{PositionedParameters, Session}
import slaq.util.NamingContext

final class DeleteInvoker[T](
  query: Query[? <: Table[T], T], profile: Profile
) {

  final protected lazy val built =
    profile.buildDelete(query, NamingContext())

  final def deleteStatement = built.sql

  def delete(using session: Session): Int =
    session.withPreparedStatement(deleteStatement) { st =>
      built.setter(new PositionedParameters(st), null)
      st.executeUpdate()
    }
  def deleteInvoker: this.type = this
}
