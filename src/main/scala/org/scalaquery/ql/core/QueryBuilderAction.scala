package org.scalaquery.ql.core

import scala.collection.mutable.LinkedHashMap
import org.scalaquery.ql._
import org.scalaquery.util._

trait QueryBuilderAction 
	extends SelectBuilder 
		 with FromBuilder
		 with UpdateBuilder 
		 with DeleteBuilder { self: QueryBuilder =>
		
  private val subQueries = new LinkedHashMap[RefId[Query[_,_]], Self]
  protected val tableAliases = new LinkedHashMap[String, Table.Ref]
	protected var selectSlot: SqlBuilder = _
  protected var fromSlot:   SqlBuilder = _
  protected var nc: NamingContext = namingContext

  protected def createSubQueryBuilder(q: Query[_,_], nc: NamingContext): Self
	
  final def buildSelect(b: SqlBuilder): Unit = Select.build(b)
  final def buildSelect: 
  	(SqlBuilder.Result, ValueLinearizer[_]) = Select.build
  
  final def buildUpdate = Update.build
  final def buildDelete = Delete.build
  
  final protected def subQueryBuilder(q: Query[_,_]): Self =
    subQueries.getOrElseUpdate(
    	RefId(q), createSubQueryBuilder(q, namingContext)
    )
    
	final protected def buildSubQueries(): Unit =
		for(qb <- subQueries.valuesIterator) qb.From.build
	
	protected def overrideAlias(table: Node, name: String) = {
    nc = nc.overrideName(table, name)
	}

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
}