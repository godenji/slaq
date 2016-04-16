package org.scalaquery.ql.core

import org.scalaquery.ql.{Query,Table}
import org.scalaquery.session.{PositionedParameters, Session}
import org.scalaquery.util.NamingContext

final class DeleteInvoker[T](
	query: Query[_ <: Table[T], T], profile: Profile) {

  protected lazy val built = 
  	profile.buildDeleteStatement(query, NamingContext())

  def deleteStatement = built.sql

  def delete(implicit session: Session): Int = 
  	session.withPreparedStatement(deleteStatement){st=>
    	built.setter(new PositionedParameters(st), null)
    	st.executeUpdate
  }
  def deleteInvoker: this.type = this
}
