package org.scalaquery.ql.core

import org.scalaquery.ql.{Table, Sequence, Query, DDL}
import org.scalaquery.util.{
	ValueLinearizer, NamingContext, SqlBuilder
}

trait Profile {
  type ImplicitT <: ImplicitConversions[_ <: Profile]
  type TypeMapperDelegatesT <: TypeMapperDelegates

  def createQueryTemplate[P,R](query: Query[_,R]): 
  	QueryTemplate[P,R] = new QueryTemplate[P,R](query, this)
  	
  def createQueryBuilder(query: Query[_,_], nc: NamingContext): 
  	QueryBuilder = new GenericQueryBuilder(query, nc, None, this)

  val Implicit: ImplicitT
  val typeMapperDelegates: TypeMapperDelegatesT
  val sqlUtils = new SQLUtils

  def buildSelect(query: Query[_,_], nc: NamingContext): 
  	(SqlBuilder.Result, ValueLinearizer[_]) = 
  		createQueryBuilder(query, nc).buildSelect
  		
  def buildUpdate(query: Query[_,_], nc: NamingContext): 
  	SqlBuilder.Result = createQueryBuilder(query, nc).buildUpdate
  	
  def buildDelete(query: Query[_,_], nc: NamingContext): 
  	SqlBuilder.Result = createQueryBuilder(query, nc).buildDelete

  def buildInsert(cb: Any): String = new InsertBuilder(cb, this).buildInsert
  def buildInsert(cb: Any, q: Query[_,_]): 
  	SqlBuilder.Result = new InsertBuilder(cb, this).buildInsert(q)

  def buildTableDDL(table: Table[_]): DDL = 
  	new DDLBuilder(table, this).buildDDL
  	
  def buildSequenceDDL(seq: Sequence[_]): DDL = 
  	new SequenceDDLBuilder(seq, this).buildDDL
}
