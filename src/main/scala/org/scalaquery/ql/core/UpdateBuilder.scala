package org.scalaquery.ql.core

import org.scalaquery.Fail
import org.scalaquery.ql._
import org.scalaquery.util._

trait UpdateBuilder { self: QueryBuilder with QueryBuilderAction =>
  import profile.sqlUtils._

  object Update {
    def build: SqlBuilder.Result = {
      val b = new SqlBuilder += "UPDATE "
      val tableNameSlot = b.createSlot
      b += " SET "
      var table: Node = null
      var tableName: String = null

      def setColumn(node: NamedColumn[_]): Unit = {
        val (t, tn, colName) = node match {
          case NamedColumn(t @ Table(tn), cn, _)               => (t, tn, cn)
          case NamedColumn(ta @ Table.Alias(Table(tn)), cn, _) => (ta, tn, cn)
        }
        if (tableName eq null) { tableName = tn; table = t }
        else if (tableName != tn) Fail(
          "All columns in an UPDATE statement must be from the same table"
        )
        b += quote(colName) += "=?"
      }
      def apply(node: Node): Unit = {
        def project(table: Node, name: String, columns: ColumnBase[_]) = {
          overrideAlias(table, name)
          apply(Node(columns))
        }
        node match {
          case p: Projection[_] =>
            p.nodeChildren.zipWithIndex.foreach {
              case (n: NamedColumn[_], i) =>
                if (i > 0) b += ','
                setColumn(n)
              case _ =>
            }
          case t @ Table(name)                   => project(t, name, t.*)
          case ta @ Table.Alias(t @ Table(name)) => project(ta, name, t.*)
          case n: NamedColumn[_]                 => setColumn(n)
          case n =>
            Fail(s"""
		        	Cannot create UPDATE statement from $n; 
							A single named column, or a projection of named columns,
							from the same table is required""")
        }
      }
      apply(query.reified)
      overrideAlias(table, tableName) // update statement does not support aliasing
      tableNameSlot += quote(tableName)
      appendConditions(b)
      if (tableAliases.size > 1) Fail(
        "An UPDATE statement must not use more than one table at the top level"
      )
      b.build
    }
  }
}