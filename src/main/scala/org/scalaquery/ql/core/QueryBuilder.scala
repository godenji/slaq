package org.scalaquery.ql.core

import scala.collection.mutable.{
	LinkedHashMap, LinkedHashSet
}
import org.scalaquery.SQueryException
import org.scalaquery.ql._
import org.scalaquery.util._

class ConcreteQueryBuilder(
	_query: Query[_,_], 
	_nc: NamingContext,
	parent: Option[QueryBuilder], 
	_profile: Profile
) 
extends QueryBuilder(_query, _nc, parent, _profile) {
	
  type Self = QueryBuilder
  protected def createSubQueryBuilder(
  	query: Query[_,_], nc: NamingContext) = 
  		new ConcreteQueryBuilder(query, nc, Some(this), profile)
}

abstract class QueryBuilder(
	_query: Query[_,_],
	_nc: NamingContext,
	val parent: Option[QueryBuilder],
	val _profile: Profile
)
extends QueryBuilderAction 
	with QueryBuilderClause {import _profile.sqlUtils._
  
  final def buildSelect: (SQLBuilder.Result, ValueLinearizer[_]) =
  	SelectBuilder.buildSelect
  	
	def buildSelect(b: SQLBuilder) = 
		SelectBuilder.buildSelect(b)
		
  protected def innerBuildSelect(b: SQLBuilder, rename: Boolean): Unit =
  	SelectBuilder.innerBuildSelect(b, rename)
  	
  def buildUpdate = UpdateBuilder.buildUpdate
  def buildDelete = DeleteBuilder.buildDelete
  
  def insertAllFromClauses(): Unit = FromBuilder.insertAllFromClauses()

  //TODO Pull tables out of subqueries where needed
  type Self <: QueryBuilder

  protected val profile = _profile
  protected val query: Query[_,_] = _query
  protected var nc: NamingContext = _nc
  protected val localTables = new LinkedHashMap[String, Node]
  protected val declaredTables = new LinkedHashSet[String]
  protected val subQueryBuilders = new LinkedHashMap[RefId[Query[_,_]], Self]
  protected var fromSlot: SQLBuilder = _
  protected var selectSlot: SQLBuilder = _
  protected var maxColumnPos = 0

  protected val mayLimit0 = true
  protected val scalarFrom: Option[String] = None
  protected val supportsTuples = true
  protected val supportsCast = true
  protected val concatOperator: Option[String] = None

  protected def createSubQueryBuilder
  	(query: Query[_,_], nc: NamingContext): Self

  protected def localTableName(node: Node) = node match {
    case JoinPart(left,right) =>
      localTables(nc.nameFor(right)) = right
      nc.nameFor(left)
    case _=>
      val alias = nc.nameFor(node)
      //println(s"localTableName() >> node ${node}, alias $alias")
      localTables(alias) = node
      alias
  }

  def isDeclaredTable(name: String): Boolean = (
  	declaredTables contains name) || parent.map(_.isDeclaredTable(name)
  ).getOrElse(false)
  
  protected def subQueryBuilderFor(q: Query[_,_]): Self =
    subQueryBuilders.getOrElseUpdate(RefId(q), createSubQueryBuilder(q, nc))

  def expr(c: Node, b: SQLBuilder): Unit = {
  	expr(c, b, false, false)
  }

  protected def expr(c: Node, b: SQLBuilder, rename: Boolean, topLevel: Boolean) {
    var pos = 0
    c match {
      case p: ProductNode => {
        p.nodeChildren.foreach { c =>
          if(pos != 0) b += ','
          pos += 1
          expr(c, b, false, true)
          if(rename) b += s" as ${quote(s"c$pos")}"
        }
      }
      case _ => innerExpr(c, b)
    }
    if(rename && pos == 0) {
      b += s" as ${quote("c1")}"
      pos = 1
    }
    if(topLevel) this.maxColumnPos = pos
  }

  protected def innerExpr(c: Node, b: SQLBuilder): Unit = c match {
    case ConstColumn(null) => b += "NULL"
    
    case ColumnOps.Not(ColumnOps.Is(l, ConstColumn(null)))=> 
    	b += '('; expr(l, b); b += " IS NOT NULL)"
    	
    case ColumnOps.Not(e) => 
    	b += "(NOT "; expr(e, b); b+= ')'
    	
    case ColumnOps.InSet(e,seq,tm,bind)=> 
    	if(seq.isEmpty) expr(ConstColumn(false), b) else {
      	b += '('; expr(e, b); b += " IN ("
      	if(bind) b.sep(seq, ",")(x=> b +?= {(p,param)=> tm(profile).setValue(x, p)})
      	else b += seq.map(tm(profile).value2SQLLiteral).mkString(",")
      	b += "))"
    	}
    case ColumnOps.Is(l, ConstColumn(null)) => 
    	b += '('; expr(l, b); b += " IS NULL)"
    	
    case ColumnOps.Is(l, r) => 
    	b += '('; expr(l, b); b += '='; expr(r, b); b += ')'
    	
    case EscFunction("concat", l, r) if concatOperator.isDefined=>
      b += '('; expr(l, b); b += concatOperator.get; expr(r, b); b += ')'
      
    case s: SimpleFunction =>
      if(s.scalar) b += "{fn "
      b += s.name += '('; b.sep(s.nodeChildren, ",")(expr(_, b)); b += ')'
      if(s.scalar) b += '}'
      
    case SimpleLiteral(w)=> b += w
    case s: SimpleExpression=> s.toSQL(b, this)
    case ColumnOps.Between(left,start,end)=>
    	expr(left, b); b += " BETWEEN "; expr(start, b); b += " AND "; expr(end, b)
    	
    case ColumnOps.CountDistinct(e) => 
    	b += "COUNT(DISTINCT "; expr(e, b); b += ')'
    	
    case ColumnOps.Like(l,r,esc)=>
      b += '('; expr(l, b); b += " LIKE "; expr(r, b);
      esc.foreach { ch =>
        if(ch == '\'' || ch == '%' || ch == '_') throw new SQueryException(
        	s"Illegal escape character '$ch' for LIKE expression"
        )
        b += s" escape '$ch'"
      }
      b += ')'
      
    case a @ ColumnOps.AsColumnOf(ch,name)=>
      val tn = name.getOrElse(mapTypeName(a.typeMapper(profile)))
      if(supportsCast)
      	{b += "CAST("; expr(ch, b); b += s" as $tn)"} 
      else
      	{b += "{fn convert("; expr(ch, b); b += s", $tn)}"}
      
    case s: SimpleBinaryOperator=>
    	b += '('; expr(s.left, b); b += ' ' += s.name += ' '; expr(s.right, b); b += ')'
    	
    case q:Query[_,_]=>
    	b += "("; subQueryBuilderFor(q).innerBuildSelect(b, false); b += ")"
    	
    case c @ ConstColumn(v)=>
    	b += c.typeMapper(profile).value2SQLLiteral(v)
    	
    case c @ BindColumn(v)=>
    	b +?= {(p,param)=> c.typeMapper(profile).setValue(v, p)}
    	
    case pc @ ParameterColumn(idx)=>
    	b +?= {(p,param)=>
      	val v = (
      		if(idx == -1) param 
      		else param.asInstanceOf[Product].productElement(idx)
      	)
      	pc.typeMapper(profile).setValue(v, p)
    	}
    case c: Case.CaseColumn[_] =>
      b += "(CASE"
      c.clauses.foldRight(()) {(w,_)=>
        b += " WHEN "; expr(w.left, b); b += " THEN "; expr(w.right, b)
      }
      c.elseClause match {
        case ConstColumn(null) =>
        case n => b += " ELSE "; expr(n, b)
      }
      b += " END)"
      
    case n: NamedColumn[_]=>
    	b += s"${quote(localTableName(n.table))}.${quote(n.name)}"
    	
    case HavingColumn(x) => expr(c,b)
    	
    case SubqueryColumn(pos,sq,_)=> 
    	b += s"${quote(localTableName(sq))}.${quote(s"c$pos")}"
    	
    case sq @ Subquery(_,_)=> 
    	b += s"${quote(localTableName(sq))}.*"
    
    // implicit joins
    case a @ Table.Alias(t: WithOp)=> expr(t.mapOp(_ => a), b)
    case t: Table[_] => expr(Node(t.*), b)
    
    // Union
    case Table.Alias(ta: Table.Alias)=> expr(ta, b)
    	
    case fk: ForeignKey[_,_] =>
      if(supportsTuples) {
        b += "(("; expr(fk.left, b); b += ")=("; expr(fk.right, b); b += "))"
      } else {
        val cols = fk.linearizedSourceColumns zip fk.linearizedTargetColumns
        b += "("
        b.sep(cols, " AND "){ case (l,r) => expr(l, b); b += "="; expr(r, b) }
        b += ")"
      }
    case JoinPart(left,right)=>
    	//left.dump("left-dump", nc); right.dump("right-dump", nc)
    	throw new SQueryException("""
				Join queries require yield clause to reference a table's
				star projection or single column"""
    	)
    case _ =>
    	throw new SQueryException(
    		s"Don't know what to do with node `$c` in an expression"
    	)
  }

  protected def table(table: Node, alias: String, b: SQLBuilder): Unit = {
  	def show(t: Table[_]) = {
  		t.schemaName.foreach(b += quote(_) += '.')
    	b += s"${quote(t.tableName)} ${quote(alias)}"
  	}
  	table match {
	    case Table.Alias(t: Table[_]) => show(t)
	    case t: Table[_] => show(t)
	    case 
	    	Subquery(sq: Query[_,_], rename)=>
	    		b += s"(${subQueryBuilderFor(sq).innerBuildSelect(b, rename)}) ${quote(alias)}"
	    case 
	    	Subquery(Union(all, sqs), rename) => {
		      b += s"($lp"
		      var first = true
		      for(sq <- sqs) {
		        if(!first) b += (if(all) s"$rp UNION ALL $lp" else s"$rp UNION $lp")
		        subQueryBuilderFor(sq).innerBuildSelect(b, first && rename)
		        first = false
		      }
		      b += s")$rp ${quote(alias)}"
		    }
	    case j: Join[_,_] => createJoin(j, b)
	    case _ => //println(s"table() >> could not match node $t")
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

  /*
   * Join takes the following forms:
   * 	1) Join[tleft,tRight] i.e. first join is between 2 tables
   * 	2) Join[tLeft, Join[tleft,tRight]]
   * 	3) Join[Join[tleft,tRight], tRight]
   */
  protected def createJoin(j: Join[_,_], b: SQLBuilder, isFirst: Boolean = true) {
    val(left,right) = (j.leftNode, j.rightNode)
    //println(s"left = ${left}; right = ${right}")
    
    // FROM table precedes join clause
    if(isFirst) table(left, nc.nameFor(left), b)
    b += s" ${j.joinType.sqlName} JOIN "
    
    // and JOIN tables come after join clause
    if(!isFirst) matchNode(left,b)
    matchNode(right,b)
    
    b += " ON "
    expr(j.on, b)
  }
  private def matchNode(n: Node, b: SQLBuilder) = n match{
  	case t: Table[_]=> Some(table(n, nc.nameFor(n), b))
  	case _=> None
  }
  
}
