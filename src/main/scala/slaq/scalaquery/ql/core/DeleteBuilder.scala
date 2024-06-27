package slaq.ql.core

import slaq.Fail
import slaq.ql._
import slaq.util._

trait DeleteBuilder { self: QueryBuilder & QueryBuilderAction =>
  import profile.sqlUtils._

  object Delete {
    private def prefixSchema(t: Table[?]): String = {
      t.schemaName
        .map(schema => s"${quote(schema)}.")
        .getOrElse("")
    }

    def build: SqlBuilder.Result = {
      val b = new SqlBuilder += "DELETE FROM "
      val (table, tableName, schemaName) = query.reified match {
        case ta @ Table.Alias(t: Table[_]) => (ta, t.tableName, prefixSchema(t))
        case t: Table[_]                   => (t, t.tableName, prefixSchema(t))
        case n =>
          Fail(s"Cannot create DELETE statement with $n; table required")
      }
      b += s"${schemaName}${quote(tableName)}"
      overrideAlias(table, tableName) // delete statement does not support aliasing
      appendConditions(b)
      if (tableAliases.size > 1) Fail(
        "Conditions of a DELETE statement must not reference other tables"
      )
      buildSubQueries()
      b.build
    }
  }
}
