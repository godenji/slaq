package org.scalaquery.ql.core

import scala.collection.mutable.LinkedHashSet
import org.scalaquery.Fail
import org.scalaquery.ql._
import org.scalaquery.util._

trait FromBuilder { self: QueryBuilder with QueryBuilderAction =>
	import profile.sqlUtils._
	
	private val declaredTables = new LinkedHashSet[String]
	final def isDeclaredTable(name: String): Boolean = ( 
		declaredTables.exists(_ == name) || parent.exists(_.isDeclaredTable(name))
	)
	
  object From {
		def build: Unit = {
	    if(fromSlot ne null) insertFromClauses()
	    buildSubQueries()
	  }
	
	  private def insertFromClauses(): Unit = {
	    //println(tableAliases)
	    val(currentSelect, numAliases) = (
	    	selectSlot.build.sql, tableAliases.size
	    )
	    tableAliases.zipWithIndex.foreach{case((alias, tr: Table.Ref), i) =>
	    	val(hasParentAlias, isFirst) = (
	    		parent.exists(_.isDeclaredTable(alias)), 
	    		i == 0 || (i == 1 && currentSelect == "SELECT 1") // i.e. where exists
	    	)
	      if(!hasParentAlias) {
	        if(isFirst) fromSlot += " FROM "
	        tr.tableJoin.map(createJoin(_, isFirst, numAliases)(fromSlot)).
	        getOrElse{
	        	if(!isFirst) fromSlot += ','
	        	tableLabel(tr.table, alias)(fromSlot)
	        }
	        declaredTables += alias
	      }
	    }
	    if(fromSlot.isEmpty) scalarFrom.foreach(s=> fromSlot += " FROM " += s)
	  }
	  
	  private def createJoin( // numAliases == 1 == single column selected in query
	  	j: Join, isFirst: Boolean, numAliases: Int)(implicit b: SqlBuilder): Unit = {
	  	
	  	val leftAlias = nc.aliasFor(j.left)
	    if(isFirst) tableLabel(j.left, leftAlias)
	    if(!isFirst || numAliases == 1) {
		    b += s" ${j.joinType.sqlName} JOIN "
		    tableLabel(j.right, nc.aliasFor(j.right))
		    b += " ON "
		    j.on match {
		    	case q: ForeignKeyQuery[_,_] =>
		    		q.fks.foreach{fk=> // handle alias mismatch (fk table not same instance)
		    			val name = quote(fk.right.asInstanceOf[NamedColumn[_]].name)
		    			b += s"(${quote(leftAlias)}.$name = "; show(fk.left, b); b += ")"
		    		}
		    	case x => show(x, b)
		    }
	    }
	  }
	  
	  private def tableLabel(table: Node, alias: String)(implicit b: SqlBuilder): Unit = {
	  	def show(t: Table[_]) = {
	  		t.schemaName.foreach(b += quote(_) += '.')
	    	b += s"${quote(t.tableName)} ${quote(alias)}"
	  	}
	  	table match {
		    case Table.Alias(t: Table[_]) => show(t)
		    case t: Table[_] => show(t)
		    case 
		    	Subquery(Union(all, sqs), rename) =>
			      b += s"($lp"
			      var first = true
			      for(sq <- sqs) {
			        if(!first) b += (if(all) s"$rp UNION ALL $lp" else s"$rp UNION $lp")
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
	  private val(lp,rp) = profile match{
			case driver.SQLiteDriver => ("","")
			case _=> ("(",")")
		}
	}
}