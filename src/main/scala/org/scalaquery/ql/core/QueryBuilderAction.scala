package org.scalaquery.ql.core

import scala.collection.mutable.{LinkedHashMap, LinkedHashSet}
import org.scalaquery.SQueryException
import org.scalaquery.ql._
import org.scalaquery.util._

trait QueryBuilderAction {self: QueryBuilder=>
	import _profile.sqlUtils._
	
	object SelectBuilder{
		def buildSelect: (SQLBuilder.Result, ValueLinearizer[_]) = {
	    val b = new SQLBuilder
	    buildSelect(b)
	    (b.build, query.linearizer)
	  }
	
	  def buildSelect(b: SQLBuilder): Unit = {
	    innerBuildSelect(b, false)
	    insertAllFromClauses()
	  }
	
	  def innerBuildSelect(b: SQLBuilder, rename: Boolean): Unit = {
	    def inner = {
	      selectSlot = b.createSlot
	      selectSlot += "SELECT "
	      expr(query.reified, selectSlot, rename, true)
	      fromSlot = b.createSlot
	      appendClauses(b)
	    }
	    if(!mayLimit0) {
	      query.typedModifiers[TakeDrop] match {
	        case TakeDrop(Some(ConstColumn(0)),_,_) :: _=>
	        	b += "SELECT * FROM ("; inner; b += ") t0 WHERE 1=0"
	        case _=> inner
	      }
	    } else inner
	  }
	}
	
	object FromBuilder{
		def insertAllFromClauses(): Unit = {
	    if(fromSlot ne null) insertFromClauses()
	    for(qb <- subQueryBuilders.valuesIterator) qb.insertAllFromClauses()
	  }
	
	  protected def insertFromClauses() {
	    var(isFirst,isJoin) = (true,false)
	    for((name,t) <- new LinkedHashMap ++= localTables) {
	      if(!parent.map(_.isDeclaredTable(name)).getOrElse(false)) {
	        if(isFirst) { fromSlot += " FROM "; isFirst = false }
	        else t match{
        		case j:Join[_,_] => // don't comma delimit join clauses
        			isJoin = true
        			createJoin(j, fromSlot, isFirst = false)
        		case _ => 
        			fromSlot += ','
        	}
	        if(!isJoin) table(t, name, fromSlot)
	        declaredTables += name
	      }
	    }
	    if(fromSlot.isEmpty) 
	    	scalarFrom.foreach(s=> fromSlot += " FROM " += s)
	  }
	}
	
	object UpdateBuilder {
		def buildUpdate = {
	    if(!query.modifiers.isEmpty)
	    	throw new SQueryException("""
					A query for an UPDATE statement must not have any modifiers
					other than WHERE restrictions"""
	    	)
	    val b = new SQLBuilder += "UPDATE "
	    val tableNameSlot = b.createSlot
	    b += " SET "
	    var tableName: String = null
	    var table: Node = null
	    
	    def handleColumn(node: Node) {
	      (node match {
	        case nc @ NamedColumn(t @ Table(tn), n, _) => 
	        	(tn, n, nc.typeMapper, t)
	        case nc @ NamedColumn(t @ Table.Alias(Table(tn)), n, _) => 
	        	(tn, n, nc.typeMapper, t)
	        case n => throw new SQueryException(s"""
	        	Cannot create an UPDATE statement from a $n expression; 
						A single named column or a projection of named columns from
						the same aliased or base table is required
					""")
	      }) match {case(tn,n,tm,t)=>
	        if(tableName eq null) {tableName = tn; table = t}
	        else if(tableName != tn) throw new SQueryException(
	        	"All columns for an UPDATE statement must be from the same table"
	        )
	        b += quote(n) += "=?"
	      }
	    }
	    def handleColumns(node: Node) {
	      node match {
	        case p: Projection[_] =>
	          var i = 0
	          for(ch <- p.nodeChildren) {
	            if(i > 0) b += ','
	            handleColumn(ch)
	            i += 1
	          }
	        case t @ Table(tn) =>
	          nc = nc.overrideName(t, tn)
	          handleColumns(Node(t.*))
	        case a @ Table.Alias(t @ Table(tn)) =>
	          nc = nc.overrideName(a, tn)
	          handleColumns(Node(t.*))
	        case n => handleColumn(n)
	      }
	    }
	
	    handleColumns(query.reified)
	    nc = nc.overrideName(table, tableName) // Alias table to itself because UPDATE does not support aliases
	    tableNameSlot += quote(tableName)
	    appendConditions(b)
	    if(localTables.size > 1) throw new SQueryException(
	    	"An UPDATE statement must not use more than one table at the top level"
	    )
	    b.build
	  }
	}
	
	object DeleteBuilder {
		def buildDelete = {
	    val b = new SQLBuilder += "DELETE FROM "
	    val (delTable, delTableName) = query.reified match {
	      case t @ Table.Alias(base:Table[_]) => 
	      	(t, base.tableName)
	      case t:Table[_] => 
	      	(t, t.tableName)
	      case n => throw new SQueryException(s"""
					Cannot create a DELETE statement from an $n expression
					An aliased or base table is required"""
	      )
	    }
	    b += quote(delTableName)
	    
	    // Alias table to itself because DELETE does not support aliases
	    nc = nc.overrideName(delTable, delTableName)
	    
	    appendConditions(b)
	    if(localTables.size > 1) throw new SQueryException(
	    	"Conditions of a DELETE statement must not reference other tables"
	    )
	    for(qb <- subQueryBuilders.valuesIterator) qb.insertAllFromClauses()
	    b.build
	  }
	}
}