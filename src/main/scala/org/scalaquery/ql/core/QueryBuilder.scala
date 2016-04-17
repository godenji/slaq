package org.scalaquery.ql.core

import scala.collection.mutable.{LinkedHashMap, LinkedHashSet}

import org.scalaquery.Fail
import org.scalaquery.ql._
import org.scalaquery.util._

/**
 * generic query builder (benchmarking and statement verification)
 */
final class GenericQueryBuilder[T](
	_query: Query[_,_],
	_nc: NamingContext,
	_parent: Option[QueryBuilder],
	_profile: T)(implicit ev: T =:= Profile) 
	extends QueryBuilder(_query, _nc, _parent, _profile) {
	
  type Self = QueryBuilder
  protected def createSubQueryBuilder(query: Query[_,_], nc: NamingContext) = 
		new GenericQueryBuilder(query, nc, Some(this), profile)
}

abstract class QueryBuilder(
	_query: Query[_,_],
	_nc: NamingContext,
	val parent: Option[QueryBuilder],
	val _profile: Profile
)
extends QueryBuilderAction with QueryBuilderClause {
	import _profile.sqlUtils._
	
  final def buildSelect: (SQLBuilder.Result, ValueLinearizer[_]) =
  	SelectBuilder.buildSelect
  	
	def buildSelect(b: SQLBuilder) = 
		SelectBuilder.buildSelect(b)
		
  protected def innerBuildSelect(b: SQLBuilder, rename: Boolean): Unit =
  	SelectBuilder.innerBuildSelect(b, rename)
  	
  def buildUpdate = UpdateBuilder.buildUpdate
  def buildDelete = DeleteBuilder.buildDelete
  
  def insertAllFromClauses(): Unit = FromBuilder.insertAllFromClauses()

  protected def createSubQueryBuilder
  	(query: Query[_,_], nc: NamingContext): Self
	
	type Self <: QueryBuilder

  protected val profile = _profile
  protected val query: Query[_,_] = _query
  protected var nc: NamingContext = _nc
  protected val tableAliases = new LinkedHashMap[String, Table.Ref]
  protected val subQueryBuilders = new LinkedHashMap[RefId[Query[_,_]], Self]
  protected var fromSlot: SQLBuilder = _
  protected var selectSlot: SQLBuilder = _
  protected var maxColumnPos = 0

  protected val scalarFrom: Option[String] = None
  protected val concatOperator: Option[String] = None

  private final def tableAlias(node: Node): String = { 
		val alias = nc.aliasFor(node)
  	val maybeJoin = node match {
	    case Table.Alias(t: Table[_]) => t.tableJoin
	    case _ => None
	  }
		val alias2 = 
			maybeJoin match { // handle left table alias mismatch in on clause of FK join   
	  		case Some(Join(left, _, on: ForeignKeyQuery[_,_], _))
	  			if node == left => nc.aliasFor(left)
	  		case _ => alias
			}
		tableAliases.getOrElseUpdate(alias2, Table.Ref(node, maybeJoin))
		alias2
	}
  
  protected def subQueryBuilderFor(q: Query[_,_]): Self =
    subQueryBuilders.getOrElseUpdate(RefId(q), createSubQueryBuilder(q, nc))

  protected def expr(node: Node, b: SQLBuilder, rename: Boolean): Unit = {
    var pos = 0
    def alias(as: String, outer: Boolean): Unit = {
    	if(rename) b += as
    	if(outer) pos = 1
    }
    node match {
      case p: ProductNode =>
      	//println(p)
	      //p.nodeChildren.foreach(println)
        p.nodeChildren.zipWithIndex.foreach{case(n,i) =>
          if(pos != 0) b += ','
          if(n.isInstanceOf[Join]) expr(
      			p.product.productElement(i).asInstanceOf[Table[_]], b
      		)
      		else expr(n, b, false)
      		pos += 1
          alias(s" as ${quote(s"c$pos")}", false)
    		}
      case n => show(n, b)
    }
    if(pos == 0) alias(s" as ${quote("c1")}", true)
  }
  def expr(c: Node, b: SQLBuilder): Unit = expr(c, b, false)
  
  protected def show(c: Node, b: SQLBuilder): Unit = c match {
	  case c: Query[_,_]        => show(c, b)
  	case c: OperatorColumn[_] => show(c, b)
  	case c: Column[_]         => show(c, b)    
    case t: Table[_] => expr(Node(t.*), b)
    case ta @ Table.Alias(t: Table[_]) => expr(t.mapOp(_ => ta), b)
    case Table.Alias(ta: Table.Alias) => expr(ta, b) // Union
    case sq @ Subquery(_,_) => b += s"${quote(tableAlias(sq))}.*"
    case SubqueryColumn(pos,q,_) => b += s"${quote(tableAlias(q))}.${quote(s"c$pos")}"	
    case SimpleLiteral(w) => b += w
    case _ =>
    	Fail(s"Don't know what to do with node `$c` in an expression")
  }
  
  /*
   * Query show
   */
  private final def show(c: Query[_,_], b: SQLBuilder): Unit = c match {
  	case q: ForeignKeyQuery[_,_] => q.fks.foreach(show(_, b))
    case q =>
    	b += "("; subQueryBuilderFor(q).innerBuildSelect(b, false); b += ")"
  }

  /*
   * OperatorColumn show
   */
  private final def show[T](c: OperatorColumn[T], b: SQLBuilder): Unit = c match {
    case ColumnOps.Is(l, ConstColumn(null)) => b += '('; expr(l, b); b += " IS NULL)"
    case ColumnOps.Is(l,r) => b += '('; expr(l, b); b += " = "; expr(r, b); b += ')'
  	case ColumnOps.Not(
  		ColumnOps.Is(l, ConstColumn(null))) => b += '('; expr(l, b); b += " IS NOT NULL)"
    case ColumnOps.Not(e) => b += "(NOT "; expr(e, b); b+= ')'
    
    case fk: ForeignKey[_,_] =>
      b += "(("; expr(fk.left, b); b += ") = ("; expr(fk.right, b); b += "))"
      
    case ColumnOps.InSet(e,seq,tm,bind) => 
    	if(seq.isEmpty) show(ConstColumn(false), b) else {
      	b += '('; expr(e, b); b += " IN ("
      	if(bind) b.sep(seq, ",")(x=> b +?= {(p,param) => tm(profile).setValue(x, p)})
      	else b += seq.map(tm(profile).value2SQLLiteral).mkString(",")
      	b += "))"
    	}
    case ColumnOps.Between(left,start,end) =>
    	expr(left, b); b += " BETWEEN "; expr(start, b); b += " AND "; expr(end, b)
    	
    case ColumnOps.CountDistinct(e) => b += "COUNT(DISTINCT "; expr(e, b); b += ')'
    case ColumnOps.Like(l,r,esc) =>
      b += '('; expr(l, b); b += " LIKE "; expr(r, b);
      esc.foreach { ch =>
        if(ch == '\'' || ch == '%' || ch == '_') Fail(
        	s"Illegal escape character '$ch' for LIKE expression"
        )
        b += s" escape '$ch'"
      }
      b += ')'
    	
    case EscFunction("concat", l, r) if concatOperator.isDefined =>
      b += '('; expr(l, b); b += concatOperator.get; expr(r, b); b += ')'
      
    case s: SimpleFunction =>
      if(s.scalar) b += "{fn "
      b += s.name += '('; b.sep(s.nodeChildren, ",")(expr(_, b)); b += ')'
      if(s.scalar) b += '}'
      
    case s: SimpleExpression=> s.toSQL(b, this)
    case s: SimpleBinaryOperator=>
    	b += '('; expr(s.left, b); b += s" ${s.name} "; expr(s.right, b); b += ')'
  }
  
  /*
   * Column show
   */
  private final def show[T](c: Column[T], b: SQLBuilder): Unit = c match {
    case n: NamedColumn[_] =>
    	b += s"${quote(tableAlias(n.table))}.${quote(n.name)}"
    	
    case c @ BindColumn(v) =>
    	b +?= {(p,param) => c.typeMapper(profile).setValue(v, p)}
    	
    case pc @ ParameterColumn(idx) =>
    	b +?= {(p,param) =>
      	val v = (
      		if(idx == -1) param 
      		else param.asInstanceOf[Product].productElement(idx)
      	)
      	pc.typeMapper(profile).setValue(v.asInstanceOf[T], p)
    	}
    	
  	case ConstColumn(null) => b += "NULL"
    case c @ ConstColumn(v) => b += c.typeMapper(profile).value2SQLLiteral(v)
    case HavingColumn(x) => expr(c,b)
    case a @ AsColumnOf(ch,name) =>
      val tn = name.getOrElse(mapTypeName(a.typeMapper(profile)))
      b += "cast("; expr(ch, b); b += s" as $tn)"
      
    case c: CaseColumn[_] =>
      b += "(CASE"
      c.clauses.foldRight(()) {(w,_) =>
        b += " WHEN "; expr(w.left, b); b += " THEN "; expr(w.right, b)
      }
      c.elseClause match {
        case ConstColumn(null) =>
        case n => b += " ELSE "; expr(n, b)
      }
      b += " END)"
    
    // Column is sealed, these nodes don't generate sql 
    case _: OperatorColumn[_] | _: WrappedColumn[_] =>
  }

  protected def tableLabel(table: Node, alias: String, b: SQLBuilder): Unit = {
  	def show(t: Table[_]) = {
  		t.schemaName.foreach(b += quote(_) += '.')
    	b += s"${quote(t.tableName)} ${quote(alias)}"
  	}
  	table match {
	    case Table.Alias(t: Table[_]) => show(t)
	    case t: Table[_] => show(t)
	    case 
	    	Subquery(sq: Query[_,_], rename) =>
	    		b += s"(${subQueryBuilderFor(sq).innerBuildSelect(b, rename)}) ${quote(alias)}"
	    case 
	    	Subquery(Union(all, sqs), rename) =>
		      b += s"($lp"
		      var first = true
		      for(sq <- sqs) {
		        if(!first) b += (if(all) s"$rp UNION ALL $lp" else s"$rp UNION $lp")
		        subQueryBuilderFor(sq).innerBuildSelect(b, first && rename)
		        first = false
		      }
		      b += s")$rp ${quote(alias)}"
	    case _ => println(s"tableLabel() >> could not match node $table")
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
  private val(lp,rp) = _profile match{
		case driver.SQLiteDriver => ("","")
		case _=> ("(",")")
	}
  
}
