package org.scalaquery.ql.basic

import scala.collection.mutable.{HashMap, HashSet}
import org.scalaquery.SQueryException
import org.scalaquery.ql._
import org.scalaquery.util._
import org.scalaquery.session.{PositionedParameters, PositionedResult}

class ConcreteBasicQueryBuilder(
	_query: Query[_, _], _nc: NamingContext, parent: Option[BasicQueryBuilder], 
	_profile: BasicProfile) 
	extends BasicQueryBuilder(_query, _nc, parent, _profile) {
	
  type Self = BasicQueryBuilder

  protected def createSubQueryBuilder(query: Query[_, _], nc: NamingContext) =
    new ConcreteBasicQueryBuilder(query, nc, Some(this), profile)
}

abstract class BasicQueryBuilder(
	_query: Query[_, _], _nc: NamingContext, parent: Option[BasicQueryBuilder], _profile: BasicProfile) {
  import _profile.sqlUtils._

  //TODO Pull tables out of subqueries where needed
  type Self <: BasicQueryBuilder

  protected val profile = _profile
  protected val query: Query[_, _] = _query
  protected var nc: NamingContext = _nc
  protected val localTables = new HashMap[String, Node]
  protected val declaredTables = new HashSet[String]
  protected val subQueryBuilders = new HashMap[RefId[Query[_, _]], Self]
  protected var fromSlot: SQLBuilder = _
  protected var selectSlot: SQLBuilder = _
  protected var maxColumnPos = 0

  protected val mayLimit0 = true
  protected val scalarFrom: Option[String] = None
  protected val supportsTuples = true
  protected val supportsCast = true
  protected val concatOperator: Option[String] = None

  protected def createSubQueryBuilder(query: Query[_, _], nc: NamingContext): Self

  protected def localTableName(node: Node) = node match {
    case Join.JoinPart(left,right) =>
      localTables(nc.nameFor(right)) = right
      nc.nameFor(left)
    case x: AbstractTable.Alias=> // check for existing alias when node child is JoinPart
    	val alias = nc.nameFor(node)
    	
//    	println(s"localTableName() >> ${x.child} with alias $alias of $x")
//    	x.child match{
//    		case Join.JoinPart(left,_) => 
//    			left match{
//    				case a: AbstractTable.Alias=>
//    					//println(s"${a.child} of inner $a"); println(localTables)
//    					val maybeAlias = localTables.find{
//    						case(alias,tbl:AbstractTable.Alias)=> a.child == tbl.child
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
    				case at: AbstractTable.Alias=>
    					localTables.find{
    						case(_,table:AbstractTable.Alias)=> at.child == table.child
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

  protected def isDeclaredTable(name: String): Boolean =
    (declaredTables contains name) || parent.map(_.isDeclaredTable(name)).getOrElse(false)
  
  protected def subQueryBuilderFor(q: Query[_, _]) =
    subQueryBuilders.getOrElseUpdate(RefId(q), createSubQueryBuilder(q, nc))

  protected def rewriteCountStarQuery(q: Query[_, _]): Boolean =
    q.modifiers.isEmpty && (q.reified match {
      case AbstractTable.Alias(_: AbstractTable[_]) => true
      case _: AbstractTable[_] => true
      case _ => false
    })

  @inline final def buildSelect: (SQLBuilder.Result, ValueLinearizer[_]) = {
    val b = new SQLBuilder
    buildSelect(b)
    (b.build, query.linearizer)
  }

  def buildSelect(b: SQLBuilder): Unit = {
    innerBuildSelect(b, false)
    insertAllFromClauses()
  }

  protected def innerBuildSelect(b: SQLBuilder, rename: Boolean): Unit = {
    query.reified match {
      case ColumnOps.CountAll(Subquery(q: Query[_, _], false)) if rewriteCountStarQuery(q) =>
        val newQ = q.map(p => ColumnOps.CountAll(Node(p)))
        subQueryBuilderFor(newQ).innerBuildSelect(b, rename)
      case _ => innerBuildSelectNoRewrite(b, rename)
    }
  }

  protected def innerBuildSelectNoRewrite(b: SQLBuilder, rename: Boolean): Unit = {
    def inner() {
      selectSlot = b.createSlot
      selectSlot += "SELECT "
      expr(query.reified, selectSlot, rename, true)
      fromSlot = b.createSlot
      appendClauses(b)
    }
    if(!mayLimit0) {
      query.typedModifiers[TakeDrop] match {
        case TakeDrop(Some(ConstColumn(0)),_,_) :: _ => b += "SELECT * FROM ("; inner(); b += ") t0 WHERE 1=0"
        case _ => inner()
      }
    } else inner()
  }

  protected def appendClauses(b: SQLBuilder) {
    appendConditions(b)
    appendGroupClause(b)
    appendHavingConditions(b)
    appendOrderClause(b)
    appendLimitClause(b)
  }

  protected def appendGroupClause(b: SQLBuilder): Unit = 
  	query.typedModifiers[Grouping] match {
    	case Nil =>
    	case xs => b += " GROUP BY "; b.sep(xs, ",")(x => expr(x.by, b, false, true))
  	}

  protected def appendOrderClause(b: SQLBuilder): Unit = 
  	query.typedModifiers[Ordering] match {
    	case Nil =>
    	case xs => b += " ORDER BY "; b.sep(xs, ",")(appendOrdering(_,b))
  	}

  protected def appendOrdering(o: Ordering, b: SQLBuilder): Unit = {
    expr(o.by, b, false, true)
    if(o.isInstanceOf[Ordering.Desc]) b += " desc"
    o.nullOrdering match {
      case Ordering.NullsFirst => b += " nulls first"
      case Ordering.NullsLast => b += " nulls last"
      case Ordering.NullsDefault =>
    }
  }

  protected def appendLimitClause(b: SQLBuilder): Unit = query.typedModifiers[TakeDrop].lastOption.foreach {
    /* SQL:2008 syntax */
    case TakeDrop(Some(ConstColumn(0)),_,_) if(!mayLimit0) => // handled above in innerBuildSelectNoRewrite
    case TakeDrop(Some(t), Some(d), compareNode) => 
    	appendColumnValue(b+= " OFFSET ",d); appendColumnValue(b+= " ROW FETCH NEXT ", t, compareNode); b+= " ROW ONLY"
    	
    case TakeDrop(Some(t), None, _) => 
    	appendColumnValue(b+= " FETCH NEXT ",t); b+= " ROW ONLY"
    	
    case TakeDrop(None, Some(d), _) => 
    	appendColumnValue(b+= " OFFSET ",d); b+= " ROW"
    case _ =>
  }
  
  /*
   * appends a concrete value of type ConstColumn or bound Param to a QueryModifier clause
   * @compareNode used to calculate `take x drop y` operation where take must be of value max(0, x-y)
   */
  protected def appendColumnValue(b: SQLBuilder, node: Column[Int], compareNode: Option[Column[Int]] = None): Unit = {
  	(node,compareNode) match{
  		case(x @ ConstColumn(nodeVal), None) => b+= nodeVal
  		case(t @ ConstColumn(takeVal), d @ Some(ConstColumn(dropVal))) => b+= math.max(0, takeVal - dropVal)
  		case(x @ ParameterColumn(idx), None) => b +?= {(p,param)=>
  			val nodeVal = (if(idx == -1) param else param.asInstanceOf[Product].productElement(idx)).asInstanceOf[Int]
  			x.typeMapper(profile).setValue(nodeVal, p)
  		}
  		case(takeCol @ ParameterColumn(tIdx), dropCol @ Some(ParameterColumn(dIdx))) => b +?= {(p,param)=>
  			val takeVal = (if(tIdx == -1) param else param.asInstanceOf[Product].productElement(tIdx)).asInstanceOf[Int]
  			val dropVal = (if(dIdx == -1) param else param.asInstanceOf[Product].productElement(dIdx)).asInstanceOf[Int]
      	takeCol.typeMapper(profile).setValue(math.max(0, takeVal - dropVal), p)
  		}
  		case _=> throw new SQueryException(s"""
				values in a take + drop operation cannot be mixed; they must be either ConstColumn[Int] or Param[Int].
				Supplied node $node and Optional compareNode $compareNode could not be converted to a literal value""")
  	}
  	()
  }

  def buildDelete = {
    val b = new SQLBuilder += "DELETE FROM "
    val (delTable, delTableName) = query.reified match {
      case t @ AbstractTable.Alias(base:AbstractTable[_]) => (t, base.tableName)
      case t:AbstractTable[_] => (t, t.tableName)
      case n => throw new SQueryException(
      	s"Cannot create a DELETE statement from an $n expression; An aliased or base table is required"
      )
    }
    b += quoteIdentifier(delTableName)
    nc = nc.overrideName(delTable, delTableName) // Alias table to itself because DELETE does not support aliases
    appendConditions(b)
    if(localTables.size > 1) throw new SQueryException("Conditions of a DELETE statement must not reference other tables")
    if(query.condHaving ne Nil) throw new SQueryException("DELETE statement must contain a HAVING clause")
    for(qb <- subQueryBuilders.valuesIterator) qb.insertAllFromClauses()
    b.build
  }

  def buildUpdate = {
    if(!query.condHaving.isEmpty || !query.modifiers.isEmpty) throw new SQueryException(
    	"A query for an UPDATE statement must not have any modifiers other than WHERE restrictions"
    )
    val b = new SQLBuilder += "UPDATE "
    val tableNameSlot = b.createSlot
    b += " SET "
    var tableName: String = null
    var table: Node = null
    
    def handleColumn(node: Node) {
      (node match {
        case nc @ NamedColumn(t @ AbstractTable(tn), n, _) => (tn, n, nc.typeMapper, t)
        case nc @ NamedColumn(t @ AbstractTable.Alias(AbstractTable(tn)), n, _) => (tn, n, nc.typeMapper, t)
        case n => throw new SQueryException(s"""
        	Cannot create an UPDATE statement from a $n expression; 
					A single named column or a projection of named columns from the same aliased or base table is required
				""")
      }) match { case (tn, n, tm, t) =>
        if(tableName eq null) { tableName = tn; table = t; }
        else if(tableName != tn) throw new SQueryException(
        	"All columns for an UPDATE statement must be from the same table"
        )
        b += quoteIdentifier(n) += "=?"
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
        case t @ AbstractTable(tn) =>
          nc = nc.overrideName(t, tn)
          handleColumns(Node(t.*))
        case a @ AbstractTable.Alias(t @ AbstractTable(tn)) =>
          nc = nc.overrideName(a, tn)
          handleColumns(Node(t.*))
        case n => handleColumn(n)
      }
    }

    handleColumns(query.reified)
    nc = nc.overrideName(table, tableName) // Alias table to itself because UPDATE does not support aliases
    tableNameSlot += quoteIdentifier(tableName)
    appendConditions(b)
    if(localTables.size > 1) throw new SQueryException(
    	"An UPDATE statement must not use more than one table at the top level"
    )
    b.build
  }

  def expr(c: Node, b: SQLBuilder): Unit = expr(c, b, false, false)

  protected def expr(c: Node, b: SQLBuilder, rename: Boolean, topLevel: Boolean) {
    var pos = 0
    c match {
      case p: ProductNode => {
        p.nodeChildren.foreach { c =>
          if(pos != 0) b += ','
          pos += 1
          expr(c, b, false, true)
          if(rename) b += s" as ${quoteIdentifier("c" + pos.toString)}"
        }
      }
      case _ => innerExpr(c, b)
    }
    if(rename && pos == 0) {
      b += s" as ${quoteIdentifier("c1")}"
      pos = 1
    }
    if(topLevel) this.maxColumnPos = pos
  }

  protected def innerExpr(c: Node, b: SQLBuilder): Unit = c match {
    case ConstColumn(null) => b += "null"
    case ColumnOps.Not(ColumnOps.Is(l, ConstColumn(null))) => b += '('; expr(l, b); b += " is not null)"
    case ColumnOps.Not(e) => b += "(not "; expr(e, b); b+= ')'
    case ColumnOps.InSet(e, seq, tm, bind) => if(seq.isEmpty) expr(ConstColumn(false), b) else {
      b += '('; expr(e, b); b += " IN ("
      if(bind) b.sep(seq, ",")(x => b +?= { (p, param) => tm(profile).setValue(x, p) })
      else b += seq.map(tm(profile).valueToSQLLiteral).mkString(",")
      b += "))"
    }
    case ColumnOps.Is(l, ConstColumn(null)) => b += '('; expr(l, b); b += " is null)"
    case ColumnOps.Is(l, r) => b += '('; expr(l, b); b += '='; expr(r, b); b += ')'
    case EscFunction("concat", l, r) if concatOperator.isDefined =>
      b += '('; expr(l, b); b += concatOperator.get; expr(r, b); b += ')'
    case s: SimpleFunction =>
      if(s.scalar) b += "{fn "
      b += s.name += '('; b.sep(s.nodeChildren, ",")(expr(_, b)); b += ')'
      if(s.scalar) b += '}'
    case SimpleLiteral(w) => b += w
    case s: SimpleExpression => s.toSQL(b, this)
    case ColumnOps.Between(left, start, end) => expr(left, b); b += " BETWEEN "; expr(start, b); b += " AND "; expr(end, b)
    case ColumnOps.CountAll(q) => b += "COUNT(*)"; localTableName(q)
    case ColumnOps.CountDistinct(e) => b += "COUNT(DISTINCT "; expr(e, b); b += ')'
    case ColumnOps.Like(l, r, esc) =>
      b += '('; expr(l, b); b += " LIKE "; expr(r, b);
      esc.foreach { ch =>
        if(ch == '\'' || ch == '%' || ch == '_') throw new SQueryException(s"Illegal escape character '$ch' for LIKE expression")
        // JDBC defines an {escape } syntax but the unescaped version is understood by more DBs/drivers
        b += s" escape '$ch'"
      }
      b += ')'
    case a @ ColumnOps.AsColumnOf(ch, name) =>
      val tn = name.getOrElse(mapTypeName(a.typeMapper(profile)))
      if(supportsCast)
      	{b += "CAST("; expr(ch, b); b += s" as $tn)"} 
      else
      	{b += "{fn convert("; expr(ch, b); b += s", $tn)}"}
    case s: SimpleBinaryOperator => b += '('; expr(s.left, b); b += ' ' += s.name += ' '; expr(s.right, b); b += ')'
    case query:Query[_, _] => b += "("; subQueryBuilderFor(query).innerBuildSelect(b, false); b += ")"
    //case Union.UnionPart(_) => "*"
    case c @ ConstColumn(v) => b += c.typeMapper(profile).valueToSQLLiteral(v)
    case c @ BindColumn(v) => b +?= { (p, param) => c.typeMapper(profile).setValue(v, p) }
    case pc @ ParameterColumn(idx) => b +?= { (p, param) =>
      val v = if(idx == -1) param else param.asInstanceOf[Product].productElement(idx)
      pc.typeMapper(profile).setValue(v, p)
    }
    case c: Case.CaseColumn[_] =>
      b += "(CASE"
      c.clauses.foldRight(()) { (w,_) =>
        b += " WHEN "; expr(w.left, b); b += " THEN "; expr(w.right, b)
      }
      c.elseClause match {
        case ConstColumn(null) =>
        case n => b += " ELSE "; expr(n, b)
      }
      b += " end)"
    case n: NamedColumn[_] => b += quoteIdentifier(localTableName(n.table)) += '.' += quoteIdentifier(n.name)
    case SubqueryColumn(pos, sq, _) => b += quoteIdentifier(localTableName(sq)) += "." += quoteIdentifier("c" + pos.toString)
    case sq @ Subquery(_, _) => b += quoteIdentifier(localTableName(sq)) += ".*"
    case a @ AbstractTable.Alias(t: WithOp) => expr(t.mapOp(_ => a), b)
    case t: AbstractTable[_] => expr(Node(t.*), b)
    case t: TableBase[_] => b += quoteIdentifier(localTableName(t)) += ".*"
    case fk: ForeignKey[_, _] =>
      if(supportsTuples) {
        b += "(("; expr(fk.left, b); b += ")=("; expr(fk.right, b); b += "))"
      } else {
        val cols = fk.linearizedSourceColumns zip fk.linearizedTargetColumns
        b += "("
        b.sep(cols, " AND "){ case (l,r) => expr(l, b); b += "="; expr(r, b) }
        b += ")"
      }
    case Join.JoinPart(left,right) => //left.dump("left-dump", nc); right.dump("right-dump", nc)
    	throw new SQueryException(
    		s"Join queries require yield clause to reference a table's star projection or single column"
    	)
    case _ =>
    	throw new SQueryException(s"Don't know what to do with node `$c` in an expression")
  }

  protected def appendConditions(b: SQLBuilder): Unit = query.cond match {
    case Nil =>
    case xs => b += " WHERE "; b.sep(xs, " AND ")(x => expr(Node(x), b))
  }

  protected def appendHavingConditions(b: SQLBuilder): Unit = query.condHaving match {
    case Nil =>
    case xs => b += " HAVING "; b.sep(xs, " AND ")(x => expr(Node(x), b))
  }

  protected def insertAllFromClauses() {
    if(fromSlot ne null) insertFromClauses()
    for(qb <- subQueryBuilders.valuesIterator) qb.insertAllFromClauses()
  }

  protected def insertFromClauses() {
    var first = true
    for((name, t) <- new HashMap ++= localTables) {
      if(!parent.map(_.isDeclaredTable(name)).getOrElse(false)) {
        if(first) { fromSlot += " FROM "; first = false }
        else {
        	t match{
        		case j:Join[_,_] => // don't comma delimit join clauses 
        			//println(s"insertFromClauses() >> Join, omitting comma; $name has tables ${j.left} ${j.right}")
        		case _ => fromSlot += ','
        	}
        }
        table(t, name, fromSlot)
        declaredTables += name
      }
    }
    if(fromSlot.isEmpty) scalarFrom.foreach(s => fromSlot += " FROM " += s)
  }

  protected def table(t: Node, name: String, b: SQLBuilder): Unit = t match {
    case 
    	AbstractTable.Alias(base: AbstractTable[_]) =>
      	base.schemaName.foreach(b += quoteIdentifier(_) += '.')
      	b += s"${quoteIdentifier(base.tableName)} ${quoteIdentifier(name)}"
    case 
    	base: AbstractTable[_] =>
      	base.schemaName.foreach(b += quoteIdentifier(_) += '.')
      	b += s"${quoteIdentifier(base.tableName)} ${quoteIdentifier(name)}"
    case 
    	Subquery(sq: Query[_, _], rename) =>
      	b += "("; subQueryBuilderFor(sq).innerBuildSelect(b, rename); b += ") " += quoteIdentifier(name)
    case 
    	Subquery(Union(all, sqs), rename) => {
	      b += "("
	      var first = true
	      for(sq <- sqs) {
	        if(!first) b += (if(all) " UNION ALL " else " UNION ")
	        subQueryBuilderFor(sq).innerBuildSelect(b, first && rename)
	        first = false
	      }
	      b += ") " += quoteIdentifier(name)
	    }
    case j: Join[_,_] => createJoin(j, b)
    case _ =>  //println(s"table() >> could not match node $t")
  }

  protected def createJoin(j: Join[_,_], b: SQLBuilder) {
    val l = j.leftNode
    val r = j.rightNode
    table(l, nc.nameFor(l), b)
    b += s" ${j.joinType.sqlName} JOIN "
    r match {
      case rj: Join[_,_] => createJoin(rj, b)
      case _ => 
      	table(r, nc.nameFor(r), b)
    }
    b += " ON "
    expr(j.on, b)
  }
}
