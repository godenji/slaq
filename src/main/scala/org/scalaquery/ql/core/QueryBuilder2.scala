//package org.scalaquery.ql.core
//
//import org.scalaquery.Fail
//import org.scalaquery.ql._
//import org.scalaquery.util._
//import org.scalaquery.session.PositionedParameters
//import org.scalaquery.ql.{ColumnOps => Cols}
//import scala.collection.mutable.LinkedHashMap
//
//abstract class QueryBuilder2(
//	_query: Query[_,_],
//	_nc: NamingContext,
//	val _profile: Profile) { import _profile.sqlUtils._
//	
//	//TODO Pull tables out of subqueries where needed
//  type Self <: QueryBuilder
//	
//  protected def createSubQueryBuilder
//  	(query: Query[_,_], nc: NamingContext): Self
//  	
//  protected def subQueryBuilderFor(q: Query[_,_]): Self =
//    subQueryBuilders.getOrElseUpdate(RefId(q), createSubQueryBuilder(q, nc))
//  
//  def innerBuildSelect(q: Query[_,_]): String = {
//    def inner = {
//      s"SELECT ${apply(q.reified)}"
//      //appendClauses(b)
//    }
//    inner
//  }  
//    
//	protected val profile = _profile
//	protected val query: Query[_,_] = _query
//	protected var nc: NamingContext = _nc
//	protected val subQueryBuilders = new LinkedHashMap[RefId[Query[_,_]], Self]
//	
//	protected val supportsTuples = true
//	protected val supportsCast = true
//	protected val concatOperator: Option[String] = None
//  
//	def apply(node: Node): String = node match {
//		case ConstColumn(null) => "NULL"
//		case Cols.Not(Cols.Is(l, ConstColumn(null))) => s"(${apply(l)} IS NOT NULL)"
//		case Cols.Not(e) => s"(NOT ${apply(e)})"
//		
////		case Cols.InSet(e, seq, tm, bind) =>
////			if(seq.isEmpty) apply(ConstColumn(false)) 
////			else
////      	s"(${apply(e)} IN ("
////      	if(bind) 
////      		b.sep(seq, ",")(x=> b +?= {(p,param)=> tm(profile).setValue(x, p)})
////      	else b += seq.map(tm(profile).value2SQLLiteral).mkString(",")
////      	b += "))"
//		
//		case Cols.Is(l, ConstColumn(null)) => s"${apply(l)} IS NULL)"
//		case Cols.Is(l, r) => s"(${apply(l)} = ${apply(r)})"
//		case EscFunction("concat", l, r) if concatOperator.isDefined =>
//      s"(${apply(l)}${concatOperator.get}${apply(r)})"
//      
//    case s: SimpleFunction =>
//    	val x = s"${s.name}(${QueryBuilder2.sep(s.nodeChildren, ",")(apply(_))})"
//      if(s.scalar) s"{fn $x}" else x
//      
//    case SimpleLiteral(w) => w
//		//case s: SimpleExpression=> s.toSQL(b, this)
//    
//    case Cols.Between(left, start, end) =>
//    	s"${apply(left)} BETWEEN ${apply(start)} AND ${apply(end)}"
//    
//    case Cols.CountDistinct(e) =>  s"COUNT(DISTINCT ${apply(e)})"
//    case Cols.Like(l, r, esc) =>
//    	val like = s"(${apply(l)} LIKE ${apply(r)}"
//      val escape = esc.map{ch=>
//        if(ch == '\'' || ch == '%' || ch == '_') Fail(
//        	s"Illegal escape character '$ch' for LIKE expression"
//        )
//        s"escape '$ch'"
//      }
//      s"$like ${escape.mkString})"
//    
//    case a @ Cols.AsColumnOf(ch, name) =>
//      val tn = name.getOrElse(mapTypeName( a.typeMapper(profile) ))
//      val expr = apply(ch)
//      if(supportsCast) s"CAST(${expr} as $tn)" else s"{fn convert($expr, $tn)}"
//      
//    case s: SimpleBinaryOperator =>
//    	s"(${apply(s.left)} ${s.name} ${apply(s.right)})"
//    	
////    case q: Query[_,_] =>
////    	s"(${subQueryBuilderFor(q).innerBuildSelect(q)})"
//    	
//    case c @ ConstColumn(v) => s"${c.typeMapper(profile).value2SQLLiteral(v)}"
//    
////    case c @ BindColumn(v) =>
////    	b +?= {(p,param)=> c.typeMapper(profile).setValue(v, p)}
//    
////    case pc @ ParameterColumn(idx) =>
////    	b +?= {(p,param)=>
////      	val v = (
////      		if(idx == -1) param 
////      		else param.asInstanceOf[Product].productElement(idx)
////      	)
////      	pc.typeMapper(profile).setValue(v, p)
////    	}
//    
//    case c: Case.CaseColumn[_] =>
//      val p1 = c.clauses.foldRight(()) {(w,_) =>
//        s" WHEN ${apply(w.left)} THEN ${apply(w.right)}"
//      }
//      val p2 = c.elseClause match {
//        case ConstColumn(null) => ""
//        case n => s" ELSE ${apply(n)}"
//      }
//      s"(CASE $p1 $p2 END)"
//      
////    case n: NamedColumn[_] =>
////    	b += s"${quote(tableAlias(n.table))}.${quote(n.name)}"
//      
////    case SubqueryColumn(pos, sq, _) => 
////    	b += s"${quote(tableAlias(sq))}.${quote(s"c$pos")}"
////    	
////    case sq @ Subquery(_,_) => 
////    	b += s"${quote(tableAlias(sq))}.*"
//    
//    case a @ Table.Alias(t: WithOp)=> apply(t.mapOp(_ => a))
//    case t: Table[_] => apply(Node(t.*))
//    
//    // Union
//    case Table.Alias(ta: Table.Alias)=> apply(ta)
//      
//    case fk: ForeignKey[_,_] =>
//      if(supportsTuples) s"((${apply(fk.left)}) = (${apply(fk.right)}))"
//      else {
//        val cols = fk.linearizedSourceColumns.zip(fk.linearizedTargetColumns)
//        val expr = QueryBuilder2.sep(cols, " AND "){case(l,r) => 
//        	s"${apply(l)} = ${apply(r)}"
//      	}
//        s"($expr)"
//      }
//    case JoinPart(left, right) =>
//    	//left.dump("left-dump", nc); right.dump("right-dump", nc)
//    	Fail("""
//				Join queries require yield clause to reference a table's
//				star projection or single column"""
//    	)
//    case _ => Fail(s"Don't know what to do with node `$node` in an expression")
//    
//	}
//}
//object QueryBuilder2 {
//	type Setter = (PositionedParameters, Any) => Unit
//	
//	def sep[T](sequence: Traversable[T], separator: String)(f: T => String) = {
//		sequence.foldLeft(separator){
//			case(x,xs) => s"$xs $x"
//		}
//  }
//}