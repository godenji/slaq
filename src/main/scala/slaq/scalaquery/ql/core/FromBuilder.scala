package slaq.ql.core

import scala.collection.mutable.LinkedHashSet
import slaq.Fail
import slaq.ql._
import slaq.util._

trait FromBuilder { self: QueryBuilder & QueryBuilderAction =>
  import profile.sqlUtils._

  private val declaredTables = new LinkedHashSet[String]
  final def isDeclaredTable(name: String): Boolean = (
    declaredTables.exists(_ == name) || parent.exists(_.isDeclaredTable(name))
  )

  object From {
    def build: Unit = {
      if (fromSlot ne null) insertFromClauses()
      buildSubQueries()
    }

    private def insertFromClauses(): Unit = {
      //println(tableAliases)
      val (currentSelect, numAliases) = (
        selectSlot.build.sql, tableAliases.size
      )
      tableAliases.zipWithIndex.foreach {
        case ((alias, tr: Table.Ref), i) =>
          val (hasParentAlias, isFirst) = (
            parent.exists(_.isDeclaredTable(alias)),
            i == 0 || (i == 1 && currentSelect == "SELECT 1") // i.e. where exists
          )
          if (!hasParentAlias) {
            if (isFirst) fromSlot += " FROM "
            tr.tableJoin.map(createJoin(_, isFirst, numAliases)(using fromSlot)).
              getOrElse {
                tr.table match
                  // no-op if referencing subquery column in outer expression
                  // i.e. all subquery columns reference a single table `tN`
                  // (e.g. "select t1.c1, t1.c2, ... from (...) t1")
                  case NamedColumn(n, _, _) if n.isInstanceOf[SubqueryTable] => ()
                  case t =>
                    if (!isFirst) {
                      t match
                        case Subquery(_, _, Some(joinType)) =>
                          fromSlot += s" ${joinType.sqlName} JOIN LATERAL "
                        case _ => fromSlot += ','
                    }
                    tableLabel(t, alias)(using fromSlot)
              }
            declaredTables += alias
          }
      }
      if (fromSlot.isEmpty) scalarFrom.foreach(s => fromSlot += " FROM " += s)
    }

    private def createJoin( // numAliases == 1 == single column selected in query
      j: Join, isFirst: Boolean, numAliases: Int
    )(using b: SqlBuilder): Unit = {

      val leftAlias = nc.aliasFor(j.left)
      if (isFirst) tableLabel(j.left, leftAlias)
      if (!isFirst || numAliases == 1) {
        b += s" ${j.joinType.sqlName} JOIN "
        tableLabel(j.right, nc.aliasFor(j.right))
        b += " ON "
        j.on match {
          case q: ForeignKeyQuery[_, _] =>
            q.fks.foreach { fk => // handle alias mismatch (fk table not same instance)
              val name = quote(fk.right.asInstanceOf[NamedColumn[?]].name)
              b += s"(${quote(leftAlias)}.$name = "; show(fk.left, b); b += ')'
            }
          // left outer join in UNION subquery, pull out NamedColumn pair from delegate
          case x: WrappedColumn[_] => show(x.nodeDelegate, b)
          case x => show(x, b)
        }
      }
    }

    private def tableLabel(table: Node, alias: String)(using b: SqlBuilder): Unit = {
      def show(t: Table[?]) = {
        t.schemaName.foreach(b += quote(_) += '.')
        b += s"${quote(t.tableName)} ${quote(alias)}"
      }
      table match {
        case Table.Alias(t: Table[_]) => show(t)
        case t: Table[_]              => show(t)
        case Subquery(q: Query[_, _], rename, _) =>
          b += "("
          subQueryBuilder(q).Select.build(b, rename)
          b += s") ${quote(alias)}"
          queryModifiers[Lateral]
            .find(_.ref == RefId(q))
            .foreach(x =>
              b += " ON "
              self.show(x.on, b)
            )
        case Subquery(Union(all, sqs), rename, _) =>
          b += s"($lp"
          var first = true
          for (sq <- sqs) {
            if (!first) b += (if (all) s"$rp UNION ALL $lp" else s"$rp UNION $lp")
            subQueryBuilder(sq).Select.build(b, first && rename)
            first = false
          }
          b += s")$rp ${quote(alias)}"
        case _ =>
          Fail(s"Unmatched node `$table` in `tableLabel()` call")
      }
    }
    /*
	   * hack: SQLite subqueries do not allow nested parens
	   * 	while MySQL (at least) requires Union queries in the form of:
	   *  select t1.* from (
	   *  	(select t2.id from A t2 ..) UNION (select t3.id from A t3 ..)
	   * 	) t1
	   * 	when orderBy and limit clauses are required in both selects
	   */
    private val (lp, rp) = profile match {
      case driver.SQLiteDriver => ("", "")
      case _                   => ("(", ")")
    }
  }
}
