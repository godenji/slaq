package org.scalaquery.ql.core

import scala.collection.mutable.{LinkedHashMap, LinkedHashSet}
import org.scalaquery.Fail
import org.scalaquery.ql._
import org.scalaquery.util._

trait QueryBuilderAction { self: QueryBuilder=>
	import profile.sqlUtils._
	
  private val subQueries = new LinkedHashMap[RefId[Query[_,_]], Self]
  private val tableAliases = new LinkedHashMap[String, Table.Ref]
	private val declaredTables = new LinkedHashSet[String]
	private var selectSlot: SQLBuilder = _
  private var fromSlot:   SQLBuilder = _
  private var nc: NamingContext = namingContext
  
  protected def subQueryBuilder(q: Query[_,_]): Self =
    subQueries.getOrElseUpdate(
    	RefId(q), createSubQueryBuilder(q, namingContext)
    )
	
	final def isDeclaredTable(name: String): Boolean = ( 
		declaredTables.exists(_ == name) || parent.exists(_.isDeclaredTable(name))
	)

  protected[this] final def tableAlias(node: Node): String = { 
		val(node2, maybeJoin) = node match {
			case NamedColumn(t: Table[_],_,_) =>
				(t.maybeJoin.map{case j: Join => 
					j.extractNode(t.tableName, forTableAlias = true)
				}.getOrElse(t), t.maybeJoin)
			case NamedColumn(ta @ Table.Alias(t: Table[_]),_,_) => 
				(ta, t.maybeJoin)
			case n => 
				(n, None)
		}
  	val alias  = nc.aliasFor(node2)
		tableAliases.getOrElseUpdate(alias, Table.Ref(node2, maybeJoin))
		alias
	}
	
	object SelectBuilder {
		/** build select statement for a query result set */
		def build: (SQLBuilder.Result, ValueLinearizer[_]) = {
	    val b = new SQLBuilder
	    build(b)
	    (b.build, query.linearizer)
	  }
	
		/** build select statement including from clause(s) */
	  def build(b: SQLBuilder): Unit = {
	    build(b, false)
	    FromBuilder.build
	  }
	
	  /** build select statement excluding from clause(s) */
	  def build(b: SQLBuilder, rename: Boolean): Unit = {
	  	val takeNone = queryModifiers[TakeDrop] match {
        case TakeDrop(Some(ConstColumn(0)),_,_) :: _ => true
        case _ => false
	  	}
      selectSlot = b.createSlot
      selectSlot += "SELECT "
      //query.reified.dump("")
      expr(query.reified, selectSlot, rename)
      fromSlot = b.createSlot
      if(takeNone) b += " WHERE 1=0"
      else appendClauses(b)
	  }
	}
	
	object FromBuilder {
		def build: Unit = {
	    if(fromSlot ne null) insertFromClauses()
	    for(qb <- subQueries.valuesIterator) qb.FromBuilder.build
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
	  	j: Join, isFirst: Boolean, numAliases: Int)(implicit b: SQLBuilder): Unit = {
	  	
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
	  
	  private def tableLabel(table: Node, alias: String)(implicit b: SQLBuilder): Unit = {
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
			        subQueryBuilder(sq).SelectBuilder.build(b, first && rename)
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
	
	object UpdateBuilder {
		def build = {
	    if(!query.modifiers.isEmpty)
	    	Fail("""
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
	        case n => Fail(s"""
	        	Cannot create an UPDATE statement from a $n expression; 
						A single named column or a projection of named columns from
						the same aliased or base table is required
					""")
	      }) match {case(tn,n,tm,t)=>
	        if(tableName eq null) {tableName = tn; table = t}
	        else if(tableName != tn) Fail(
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
	    if(tableAliases.size > 1) Fail(
	    	"An UPDATE statement must not use more than one table at the top level"
	    )
	    b.build
	  }
	}
	
	object DeleteBuilder {
		def build = {
	    val b = new SQLBuilder += "DELETE FROM "
	    val (delTable, delTableName) = query.reified match {
	      case t @ Table.Alias(base:Table[_]) => 
	      	(t, base.tableName)
	      case t:Table[_] => 
	      	(t, t.tableName)
	      case n => Fail(s"""
					Cannot create a DELETE statement from an $n expression
					An aliased or base table is required"""
	      )
	    }
	    b += quote(delTableName)
	    
	    // Alias table to itself because DELETE does not support aliases
	    nc = nc.overrideName(delTable, delTableName)
	    
	    appendConditions(b)
	    if(tableAliases.size > 1) Fail(
	    	"Conditions of a DELETE statement must not reference other tables"
	    )
	    for(qb <- subQueries.valuesIterator) qb.FromBuilder.build
	    b.build
	  }
	}
}