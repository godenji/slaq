package org.scalaquery.ql.core

import scala.collection.mutable.{HashMap, HashSet}
import org.scalaquery.SQueryException
import org.scalaquery.ql._
import org.scalaquery.util._
import org.scalaquery.session.{
	PositionedParameters, PositionedResult
}

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
  protected val localTables = new HashMap[String, Node]
  protected val declaredTables = new HashSet[String]
  protected val subQueryBuilders = new HashMap[RefId[Query[_,_]], Self]
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
    case Join.JoinPart(left,right) =>
      localTables(nc.nameFor(right)) = right
      nc.nameFor(left)
    case x: Table.Alias=> // check for existing alias when node child is JoinPart
    	val alias = nc.nameFor(node)
    	
//    	println(s"localTableName() >> ${x.child} with alias $alias of $x")
//    	x.child match{
//    		case Join.JoinPart(left,_) => 
//    			left match{
//    				case a: Table.Alias=>
//    					//println(s"${a.child} of inner $a"); println(localTables)
//    					val maybeAlias = localTables.find{
//    						case(alias,tbl:Table.Alias)=> a.child == tbl.child
//    						case _=> false
//    					}.map(_._1)
//    					maybeAlias.foreach{currAlias=>
//    						println(s"${a.child} already has alias $currAlias, discarding new alias $alias\n")	
//    					}
//    				case _=>
//    			}
//    		case _=>
//    	}
    	
    	// override alias when join table and previous alias already defined  
    	val modAlias = x.child match{
    		case Join.JoinPart(left,_) =>
    			left match{
    				case at: Table.Alias=>
    					localTables.find{
    						case(_,table:Table.Alias)=> at.child == table.child
    						case _=> false
    					}.map{case(firstAlias,_)=> firstAlias}.getOrElse(alias)
    				case _ => alias
    			}
    		case _ => alias
    	}
    	localTables(modAlias) = node
    	modAlias
    case _ =>
      val alias = nc.nameFor(node)
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
          if(rename) b += s" as ${quote("c" + pos.toString)}"
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
    case ConstColumn(null) => b += "null"
    
    case ColumnOps.Not(ColumnOps.Is(l, ConstColumn(null)))=> 
    	b += '('; expr(l, b); b += " is not null)"
    	
    case ColumnOps.Not(e) => 
    	b += "(not "; expr(e, b); b+= ')'
    	
    case ColumnOps.InSet(e,seq,tm,bind)=> 
    	if(seq.isEmpty) expr(ConstColumn(false), b) else {
      	b += '('; expr(e, b); b += " IN ("
      	if(bind) b.sep(seq, ",")(x=> b +?= {(p,param)=> tm(profile).setValue(x, p)})
      	else b += seq.map(tm(profile).valueToSQLLiteral).mkString(",")
      	b += "))"
    	}
    case ColumnOps.Is(l, ConstColumn(null)) => 
    	b += '('; expr(l, b); b += " is null)"
    	
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
        // JDBC defines an {escape } syntax but the unescaped version
        // is understood by more DBs/drivers
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
    	
    case query:Query[_,_]=>
    	b += "("; subQueryBuilderFor(query).innerBuildSelect(b, false); b += ")"
    	
    case c @ ConstColumn(v)=>
    	b += c.typeMapper(profile).valueToSQLLiteral(v)
    	
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
      b += " end)"
      
    case n: NamedColumn[_]=>
    	b += quote(localTableName(n.table)) += '.' += quote(n.name)
    	
    case SubqueryColumn(pos,sq,_)=> 
    	b += quote(localTableName(sq)) += "." += quote("c" + pos.toString)
    	
    case sq @ Subquery(_,_)=>
    	b += quote(localTableName(sq)) += ".*"
    	
    case a @ Table.Alias(t: WithOp)=> expr(t.mapOp(_ => a), b)
    case t: Table[_] => expr(Node(t.*), b)
    case t: TableBase[_]=>
    	b += quote(localTableName(t)) += ".*"
    	
    case fk: ForeignKey[_,_] =>
      if(supportsTuples) {
        b += "(("; expr(fk.left, b); b += ")=("; expr(fk.right, b); b += "))"
      } else {
        val cols = fk.linearizedSourceColumns zip fk.linearizedTargetColumns
        b += "("
        b.sep(cols, " AND "){ case (l,r) => expr(l, b); b += "="; expr(r, b) }
        b += ")"
      }
    case Join.JoinPart(left,right)=>
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

  protected def table(t: Node, name: String, b: SQLBuilder): Unit = t match {
    case 
    	Table.Alias(base: Table[_]) =>
      	base.schemaName.foreach(b += quote(_) += '.')
      	b += s"${quote(base.tableName)} ${quote(name)}"
    case 
    	base: Table[_] =>
      	base.schemaName.foreach(b += quote(_) += '.')
      	b += s"${quote(base.tableName)} ${quote(name)}"
    case 
    	Subquery(sq: Query[_,_], rename)=>
      	b += "(";
      		subQueryBuilderFor(sq).innerBuildSelect(b, rename); b +=
      	") " += quote(name)
    case 
    	Subquery(Union(all, sqs), rename) => {
	      b += "("
	      var first = true
	      for(sq <- sqs) {
	        if(!first) b += (if(all) " UNION ALL " else " UNION ")
	        subQueryBuilderFor(sq).innerBuildSelect(b, first && rename)
	        first = false
	      }
	      b += ") " += quote(name)
	    }
    case j: Join[_,_] => createJoin(j, b)
    case _ => //println(s"table() >> could not match node $t")
  }

  protected def createJoin(j: Join[_,_], b: SQLBuilder, isFirst: Boolean = true) {
    val(l,r) = (j.leftNode, j.rightNode)
    val(lname, rname) = (nc.nameFor(l), nc.nameFor(r))
    //println(s"left = ${j.left}; right = ${j.right}")
    
    // left join table precedes join clause when FROM table
    if(isFirst) table(l, lname, b)
    b += s" ${j.joinType.sqlName} JOIN "
    // else left table comes after join clause
    if(!isFirst) table(l, lname, b)
    
    r match {
      case rj: Join[_,_] => createJoin(rj, b, isFirst = false)
      case _=> table(r, rname, b)
    }
    b += " ON "
    expr(j.on, b)
  }
}
