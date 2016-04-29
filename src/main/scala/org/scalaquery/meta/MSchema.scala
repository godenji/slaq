package org.scalaquery.meta

import java.sql._
import org.scalaquery.ResultSetInvoker

/**
 * A wrapper for a row in the ResultSet returned by DatabaseMetaData.getSchemas().
 */
case class MSchema(schema: String, catalog: Option[String]) {
  override def toString =
  	s"MSchema(${catalog.map(x=> s"$x.").mkString("")}$schema)"
}

object MSchema {
  private[this] val m = try { classOf[DatabaseMetaData].getMethod("getSchemas", classOf[String], classOf[String]) }
    catch { case _:NoSuchMethodException => null }

  def getSchemas(catalog: Option[String], schemaPattern: Option[String]) = {
    /* to support Java pre-1.6 use: 
    if(m == null) UnitInvoker.empty
    else ResultSetInvoker[MSchema](s => 
      DatabaseMeta.invokeForRS(m, s.metaData, catalog.orNull, schemaPattern.orNull)) { r => MSchema(r<<, r<<?) }
    */
  	ResultSetInvoker[MSchema](_.metaData.getSchemas(catalog.orNull, schemaPattern.orNull)) { r => MSchema(r<<, r<<?) }
  }

  def getSchemas = ResultSetInvoker[MSchema](_.metaData.getSchemas()) { r => MSchema(r<<, r<<?) }
}
