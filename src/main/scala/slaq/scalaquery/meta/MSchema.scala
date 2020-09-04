package slaq.meta

import java.sql._
import slaq.ResultSetInvoker

/**
 * A wrapper for a row in the ResultSet returned by DatabaseMetaData.getSchemas().
 */
case class MSchema(schema: String, catalog: Option[String]) {
  override def toString =
    s"MSchema(${catalog.map(x => s"$x.").mkString("")}$schema)"
}

object MSchema {
  try { classOf[DatabaseMetaData].getMethod("getSchemas", classOf[String], classOf[String]) }
  catch { case _: NoSuchMethodException => null }

  def getSchemas(catalog: Option[String], schemaPattern: Option[String]) = {
    ResultSetInvoker[MSchema](_.metaData.getSchemas(catalog.orNull, schemaPattern.orNull)) { r => MSchema(r<<, r<<?) }
  }

  def getSchemas = ResultSetInvoker[MSchema](_.metaData.getSchemas()) { r => MSchema(r<<, r<<?) }
}
