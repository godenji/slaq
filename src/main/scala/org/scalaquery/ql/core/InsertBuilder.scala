package org.scalaquery.ql.core

import scala.collection.mutable.HashMap
import java.io.PrintWriter
import org.scalaquery.Fail
import org.scalaquery.ql._
import org.scalaquery.util._

class InsertBuilder(val column: Any, val profile: Profile) {
  import profile.sqlUtils._

  def buildInsert: String = {
    val (table, cols, vals) = buildParts
    s"INSERT INTO ${quote(table)} ($cols) VALUES ($vals)"
  }

  def buildInsert(query: Query[_,_]): SQLBuilder.Result = {
    val (table, cols, _) = buildParts
    val b = new SQLBuilder
    b += s"INSERT INTO ${quote(table)} (${cols.toString}) "
    val qb = profile.createQueryBuilder(query, NamingContext())
    qb.buildSelect(b)
    b.build
  }

  protected def buildParts: (String, StringBuilder, StringBuilder) = {
    val cols = new StringBuilder
    val vals = new StringBuilder
    var table:String = null
    def f(c: Any): Unit = c match {
      case p:Projection[_] =>
        for(i <- 0 until p.productArity)
          f(Node(p.productElement(i)))
      case t:Table[_] => f(Node(t.*))
      case n:NamedColumn[_] =>
      	val tmpTable = n.table.asInstanceOf[Table[_]].tableName
        if(table eq null) table = tmpTable
        else if(table != tmpTable) Fail(
        	"Inserts must all be to the same table"
        )
        appendNamedColumn(n, cols, vals)
      case _ => Fail(
      	"Cannot use column "+c+" in INSERT statement"
      )
    }
    f(Node(column))
    if(table eq null) Fail("No table to insert into")
    (table, cols, vals)
  }

  protected def appendNamedColumn(
  	n: NamedColumn[_], cols: StringBuilder, vals: StringBuilder) {
  	
    if(!cols.isEmpty) {
      cols append ","
      vals append ","
    }
    cols append quote(n.name)
    vals append '?'
  }
}
