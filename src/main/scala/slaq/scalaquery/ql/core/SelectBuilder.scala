package slaq.ql.core

import slaq.ql._
import slaq.util._

trait SelectBuilder { self: QueryBuilder & QueryBuilderAction =>

  object Select {

    /** build select statement for a query result set */
    def build: (SqlBuilder.Result, ValueLinearizer[?]) = {
      val b = new SqlBuilder
      build(b)
      (b.build, query.linearizer)
    }

    /** build select statement including from clause(s) */
    def build(b: SqlBuilder): Unit = {
      build(b, false)
      From.build
    }

    /** build select statement excluding from clause(s) */
    def build(b: SqlBuilder, rename: Boolean): Unit = {
      val takeNone = queryModifiers[TakeDrop] match {
        case TakeDrop(Some(ConstColumn(0)), _, _) :: _ => true
        case _ => false
      }
      selectSlot = b.createSlot
      selectSlot += "SELECT "
      //query.reified.dump("")
      expr(query.reified, selectSlot, rename)
      fromSlot = b.createSlot
      if (takeNone) b += " WHERE 1=0"
      else appendClauses(b)
    }
  }
}