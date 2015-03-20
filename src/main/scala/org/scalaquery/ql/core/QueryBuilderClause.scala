package org.scalaquery.ql.core

import org.scalaquery.SQueryException
import org.scalaquery.ql._
import org.scalaquery.util._

trait QueryBuilderClause {self: QueryBuilder=>
	protected def appendClauses(b: SQLBuilder): Unit = {
    appendConditions(b)
    appendGroupClause(b)
    appendHavingConditions(b)
    appendOrderClause(b)
    appendLimitClause(b)
  }

  protected def appendConditions(b: SQLBuilder): Unit = 
  	query.cond match {
	    case Nil =>
	    case xs => 
	    	b += " WHERE "
	    	b.sep(xs, " AND ")(x=> expr(Node(x), b))
	  }

  protected def appendGroupClause(b: SQLBuilder): Unit = 
  	query.typedModifiers[Grouping] match {
    	case Nil =>
    	case xs => 
    		b += " GROUP BY "
    		b.sep(xs, ",")(x=> expr(x.by, b, false, true))
  	}

  protected def appendHavingConditions(b: SQLBuilder): Unit = 
  	query.condHaving match {
	    case Nil =>
	    case xs => 
	    	b += " HAVING "
	    	b.sep(xs, " AND ")(x=> expr(Node(x), b))
	  }

  protected def appendOrderClause(b: SQLBuilder): Unit = 
  	query.typedModifiers[Ordering] match {
    	case Nil =>
    	case xs => 
    		b += " ORDER BY "
    		b.sep(xs, ",")(appendOrdering(_,b))
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

  protected def appendLimitClause(b: SQLBuilder): Unit = 
  	query.typedModifiers[TakeDrop].lastOption.foreach {
	    /* SQL:2008 syntax */
	    case TakeDrop(Some(ConstColumn(0)),_,_)
	    	if(!mayLimit0) => // handled in innerBuildSelect
	    		
	    case TakeDrop(Some(t), Some(d), compareNode) =>
	    	val compFn = maybeLimitNode(t,d,compareNode,_:Boolean)
	    	appendLimitValue(b+= " OFFSET ", d, compFn(true)) 
	    	appendLimitValue(b+= " ROW FETCH NEXT ", t, compFn(false))
	    	b+= " ROW ONLY"
	    	
	    case TakeDrop(Some(t), None, _) => 
	    	appendLimitValue(b+= " FETCH NEXT ",t)
	    	b+= " ROW ONLY"
	    	
	    case TakeDrop(None, Some(d), _) => 
	    	appendLimitValue(b+= " OFFSET ",d)
	    	b+= " ROW"
	    case _ =>
	  }
  
  /*
   * returns a potential comparison node for take, drop calculation
   */
  protected def maybeLimitNode(
  	takeColumn: Column[Int],
  	dropColumn: Column[Int],
  	compareColumn: Option[Column[Int]], 
  	isDrop: Boolean): Option[Column[Int]] = {
  	
  	if(isDrop){
  		if(Some(dropColumn) == compareColumn) None else Some(takeColumn)
  	}
  	else
  		if(Some(takeColumn) == compareColumn) Some(dropColumn) else None
  }
  
  /*
   * appends a concrete value of type ConstColumn or bound Param 
   * to a QueryModifier take/drop clause
   * @compareNode used to calculate limit/offset
   */
  protected def appendLimitValue(
  	b: SQLBuilder, node: Column[Int], 
  	compareNode: Option[Column[Int]] = None): Unit = {
  	
  	(node,compareNode) match{
  		case(x @ ConstColumn(v), None) => b+= v
  		
  		case(aCol @ ConstColumn(aVal), bCol @ Some(ConstColumn(bVal)))=>
  			b+= setColValue(aVal,bVal)
  			
  		case(x @ ParameterColumn(idx), None) => b +?= {(p,param)=>
  			val nodeVal = (
  				if(idx == -1) param 
  				else param.asInstanceOf[Product].productElement(idx)
  			).asInstanceOf[Int]
  			x.typeMapper(profile).setValue(nodeVal, p)
  		}
  		case(
  			targCol @ ParameterColumn(aIdx), // i.e. column to set value for
  			compCol @ Some(ParameterColumn(bIdx)))=>
  				
  			b +?= {(p,param)=>
	  			val aVal = (
	  				if(aIdx == -1) param
	  				else param.asInstanceOf[Product].productElement(aIdx)
	  			).asInstanceOf[Int]
	  			val bVal = (
	  				if(bIdx == -1) param 
	  				else param.asInstanceOf[Product].productElement(bIdx)
	  			).asInstanceOf[Int]
	      	targCol.typeMapper(profile).setValue(setColValue(aVal,bVal), p)
	  		}
  		case _=> throw new SQueryException(s"""
				values in a take, drop operation cannot be mixed; 
				they must be either ConstColumn[Int] or Param[Int].
				Supplied node $node and Optional compareNode $compareNode
				could not be converted to a literal value
			""")
  	}
  	()
  }
  private def setColValue(aVal: Int, bVal: Int): Int = {
  	if(aVal == bVal) aVal
  	else if(aVal > bVal) aVal - bVal
		else (bVal - aVal) + 1
  }
  
}