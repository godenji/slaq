package org.scalaquery.ql.driver

import java.util.UUID
import org.scalaquery.ql._
import org.scalaquery.ql.core._
import org.scalaquery.util._
import org.scalaquery.session.{PositionedResult, PositionedParameters}

class PostgresDriver extends Profile { self =>

  type ImplicitT = ImplicitConversions[PostgresDriver]
  type TypeMapperDelegatesT = PostgresTypeMapperDelegates

  val Implicit = new ImplicitConversions[PostgresDriver] {
    implicit val scalaQueryDriver = self
  }

  val typeMapperDelegates = new PostgresTypeMapperDelegates

  override def createQueryBuilder(query: Query[_,_], nc: NamingContext) = new PostgresQueryBuilder(query, nc, None, this)
  override def buildTableDDL(table: Table[_]): DDL = new PostgresDDLBuilder(table, this).buildDDL
}

object PostgresDriver extends PostgresDriver

class PostgresTypeMapperDelegates extends TypeMapperDelegates {
  override val byteArrayTypeMapperDelegate = new TypeMapperDelegates.ByteArrayTypeMapperDelegate {
    override val sqlTypeName = "BYTEA"
  }
  override val uuidTypeMapperDelegate = new TypeMapperDelegates.UUIDTypeMapperDelegate {
    override def setValue(v: UUID, p: PositionedParameters) = p.setObject(v, sqlType)
    override def setOption(v: Option[UUID], p: PositionedParameters) = p.setObjectOption(v, sqlType)
    override def nextValue(r: PositionedResult) = r.nextObject().asInstanceOf[UUID]
    override def updateValue(v: UUID, r: PositionedResult) = r.updateObject(v)
    override def value2SQLLiteral(value: UUID) = "'" + value + "'"
  }

  override val byteTypeMapperDelegate = new ByteTypeMapperDelegate

  /* PostgreSQL does not have a TINYINT type, so we use SMALLINT instead. */
  class ByteTypeMapperDelegate extends TypeMapperDelegates.ByteTypeMapperDelegate {
    override def sqlTypeName = "SMALLINT"
  }
}

class PostgresQueryBuilder(_query: Query[_,_], _nc: NamingContext, parent: Option[QueryBuilder], profile: PostgresDriver)
extends QueryBuilder(_query, _nc, parent, profile) {


  override type Self = PostgresQueryBuilder
  override protected val concatOperator = Some("||")

  protected def createSubQueryBuilder(query: Query[_,_], nc: NamingContext) =
    new PostgresQueryBuilder(query, nc, Some(this), profile)

  override protected def appendLimitClause(b: SQLBuilder) = query.typedModifiers[TakeDrop].lastOption.foreach {
    case TakeDrop(Some(t), Some(d), compareNode) => 
    	appendLimitValue(b+=" LIMIT ", t, compareNode); appendLimitValue(b+=" OFFSET ",d)
    	
    case TakeDrop(Some(t), None, _) => appendLimitValue(b+=" LIMIT ",t)
    case TakeDrop(None, Some(d), _) => appendLimitValue(b+=" OFFSET ",d)
    case _ =>
  }
}

class PostgresDDLBuilder(table: Table[_], profile: PostgresDriver) extends DDLBuilder(table, profile) {
  import profile.sqlUtils._

  protected class PostgresColumnDDLBuilder(column: NamedColumn[_]) extends ColumnDDLBuilder(column) {
    override def appendColumn(sb: StringBuilder) {
      sb append quote(column.name) append ' '
      if(autoIncrement) {
        sb append "SERIAL"
        autoIncrement = false
      }
      else sb append sqlType
      appendOptions(sb)
    }
  }

  override protected def createColumnDDLBuilder(c: NamedColumn[_]) = new PostgresColumnDDLBuilder(c)
}
