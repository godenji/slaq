package slaq.meta

import java.sql._
import slaq.{ResultSetInvoker, UnitInvoker}

/**
 * A wrapper for a row in the ResultSet returned by DatabaseMetaData.getFunctions().
 */
case class MFunction(name: MQName, remarks: String, returnsTable: Option[Boolean], specificName: String) {
  def getFunctionColumns(columnNamePattern: String = "%") =
    MFunctionColumn.getFunctionColumns(name, columnNamePattern)
}

object MFunction {
  private[this] val m = classOf[DatabaseMetaData].getMethod(
    "getFunctions", classOf[String], classOf[String], classOf[String]
  )

  def getFunctions(namePattern: MQName) = {
    /* Regular version, requires Java 1.6:
		ResultSetInvoker[MFunction](
			_.metaData.getFunctions(namePattern.catalog_?, namePattern.schema_?, namePattern.name)) { r =>
				MFunction(MQName.from(r), r<<, r.nextShort match {
					case DatabaseMetaData.functionNoTable => Some(false)
					case DatabaseMetaData.functionReturnsTable => Some(true)
					case _ => None
				}, r<<)
		}*/
    if (m == null) UnitInvoker.empty
    else ResultSetInvoker[MFunction](s =>
      try DatabaseMeta.invokeForRS(
        m, s.metaData, namePattern.catalog_?,
        namePattern.schema_?, namePattern.name
      )
      catch { case _: SQLException | _: NoSuchMethodException => null }) { r =>
      MFunction(MQName.from(r), r<<, r.nextShort() match {
        case 1 /*DatabaseMetaData.functionNoTable*/ => Some(false)
        case 2 /*DatabaseMetaData.functionReturnsTable*/ => Some(true)
        case _ => None
      }, r<<)
    }
  }
}
