package slaq.meta

import java.sql._
import slaq.ResultSetInvoker
import slaq.ql.TypeMapperDelegate

/**
 * A wrapper for a row in the ResultSet returned by DatabaseMetaData.getBestRowIdentifier().
 */
case class MBestRowIdentifierColumn(
  scope: MBestRowIdentifierColumn.Scope, column: String, sqlType: Int, typeName: String,
  columnSize: Option[Int], decimalDigits: Option[Short], pseudoColumn: Option[Boolean]
) {

  def sqlTypeName = TypeMapperDelegate.typeNames.get(sqlType)
}

object MBestRowIdentifierColumn {
  def getBestRowIdentifier(table: MQName, scope: Scope, nullable: Boolean = false) =
    ResultSetInvoker[MBestRowIdentifierColumn](
      _.metaData.getBestRowIdentifier(table.catalog_?, table.schema_?, table.name, scope.value, nullable)
    ) { r =>
        MBestRowIdentifierColumn(apply(r<<), r<<, r<<, r<<, r<<, r.skip<<, r.nextShort() match {
          case DatabaseMetaData.bestRowNotPseudo => Some(false)
          case DatabaseMetaData.bestRowPseudo => Some(true)
          case _ => None
        })
      }
      
  enum Scope(val value: Int):
    case Temporary extends Scope(DatabaseMetaData.bestRowTemporary)
    case Transaction extends Scope(DatabaseMetaData.bestRowTransaction)
    case Session extends Scope(DatabaseMetaData.bestRowSession)

  private def apply(value: Short) = value match {
    case DatabaseMetaData.bestRowTemporary   => Scope.Temporary
    case DatabaseMetaData.bestRowTransaction => Scope.Transaction
    case DatabaseMetaData.bestRowSession     => Scope.Session
  }
}
