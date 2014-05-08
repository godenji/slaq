package org.scalaquery.ql.basic

import org.scalaquery.ql._
import org.scalaquery.SQueryException
import org.scalaquery.session.{PositionedResult, PositionedParameters}
import org.scalaquery.util.Node

abstract class AbstractBasicTable[T](_schemaName: Option[String], _tableName: String) 
	extends AbstractTable[T](_schemaName, _tableName) {

  type ProfileType <: BasicProfile

  val O: BasicColumnOptions = BasicColumnOptions

  def column[C : TypeMapper](n: String, options: ColumnOption[C, ProfileType]*) = new NamedColumn[C](Node(this), n, options:_*)

  def createFinderBy[P](f: (this.type => NamedColumn[P]))(implicit profile: BasicProfile, tm: TypeMapper[P]): BasicQueryTemplate[P,T] = {
    import profile.Implicit._
    Params[P](tm).flatMap(p => Query(this).where(t => ColumnOps.Is(f(t.asInstanceOf[AbstractBasicTable.this.type]), p)))(profile)
  }

  def ddl(implicit profile: ProfileType): DDL = profile.buildTableDDL(this)
}

abstract class BasicTable[T](_schemaName: Option[String], _tableName: String) extends AbstractBasicTable[T](_schemaName, _tableName) {
  def this(_tableName: String) = this(None, _tableName)
  type ProfileType = BasicProfile
}
