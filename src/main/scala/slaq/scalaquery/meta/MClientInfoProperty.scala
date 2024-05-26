package slaq.meta

import java.sql._
import slaq.{ResultSetInvoker, UnitInvoker}

/**
 * A wrapper for a row in the ResultSet returned by DatabaseMetaData.getClientInfoProperties().
 */
case class MClientInfoProperty(name: String, maxLen: Int, defaultValue: String, description: String)

object MClientInfoProperty {
  private val m = try { classOf[DatabaseMetaData].getMethod("getClientInfoProperties") }
  catch { case _: NoSuchMethodException => null }

  def getClientInfoProperties: UnitInvoker[MClientInfoProperty] = {
    ResultSetInvoker[MClientInfoProperty](_.metaData.getClientInfoProperties()) { r =>
      MClientInfoProperty(r<<, r<<, r<<, r<<)
    }
  }
}
