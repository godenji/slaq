package slaq.ql.core

import slaq.ql.{Table, Sequence, Query, DDL}
import slaq.util.{
  ValueLinearizer,
  NamingContext,
  SqlBuilder
}

trait Profile {
  type ImplicitT <: ImplicitConversions[? <: Profile]
  type TypeMapperDelegatesT <: TypeMapperDelegates

  def createQueryTemplate[P, R](query: Query[?, R]): QueryTemplate[P, R] = new QueryTemplate[P, R](query.asInstanceOf[Query[P, R]], this)

  def createQueryBuilder(query: Query[?, ?], nc: NamingContext): QueryBuilder = new GenericQueryBuilder(query, nc, None, this)

  val Implicit: ImplicitT
  val typeMapperDelegates: TypeMapperDelegatesT
  val sqlUtils = new SQLUtils

  def buildSelect(query: Query[?, ?], nc: NamingContext): (SqlBuilder.Result, ValueLinearizer[?]) =
    createQueryBuilder(query, nc).buildSelect

  def buildUpdate(query: Query[?, ?], nc: NamingContext): SqlBuilder.Result = createQueryBuilder(query, nc).buildUpdate

  def buildDelete(query: Query[?, ?], nc: NamingContext): SqlBuilder.Result = createQueryBuilder(query, nc).buildDelete

  def buildInsert(cb: Any): String = new InsertBuilder(cb, this).buildInsert
  def buildInsert(cb: Any, q: Query[?, ?]): SqlBuilder.Result = new InsertBuilder(cb, this).buildInsert(q)

  def buildTableDDL(table: Table[?]): DDL =
    new DDLBuilder(table, this).buildDDL

  def buildSequenceDDL(seq: Sequence[?]): DDL =
    new SequenceDDLBuilder(seq, this).buildDDL
}
