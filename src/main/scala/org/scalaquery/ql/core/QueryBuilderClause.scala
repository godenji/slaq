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
	    	appendColumnValue(b+= " OFFSET ",d) 
	    	appendColumnValue(b+= " ROW FETCH NEXT ", t, compareNode)
	    	b+= " ROW ONLY"
	    	
	    case TakeDrop(Some(t), None, _) => 
	    	appendColumnValue(b+= " FETCH NEXT ",t)
	    	b+= " ROW ONLY"
	    	
	    case TakeDrop(None, Some(d), _) => 
	    	appendColumnValue(b+= " OFFSET ",d)
	    	b+= " ROW"
	    case _ =>
	  }
  
  /*
   * appends a concrete value of type ConstColumn or bound Param 
   * to a QueryModifier clause
   * @compareNode used to calculate `take x drop y` operation 
   * where take must be of value max(0, x-y)
   */
  protected def appendColumnValue(
  	b: SQLBuilder, node: Column[Int], 
  	compareNode: Option[Column[Int]] = None): Unit = {
  	
  	(node,compareNode) match{
  		case(x @ ConstColumn(nodeVal), None) => b+= nodeVal
  		
  		case(t @ ConstColumn(takeVal), d @ Some(ConstColumn(dropVal)))=> 
  			b+= math.max(0, takeVal - dropVal)
  			
  		case(x @ ParameterColumn(idx), None) => b +?= {(p,param)=>
  			val nodeVal = (
  				if(idx == -1) param 
  				else param.asInstanceOf[Product].productElement(idx)
  			).asInstanceOf[Int]
  			x.typeMapper(profile).setValue(nodeVal, p)
  		}
  		case(
  			takeCol @ ParameterColumn(tIdx), 
  			dropCol @ Some(ParameterColumn(dIdx)))=>
  				
  			b +?= {(p,param)=>
	  			val takeVal = (
	  				if(tIdx == -1) param
	  				else param.asInstanceOf[Product].productElement(tIdx)
	  			).asInstanceOf[Int]
	  			val dropVal = (
	  				if(dIdx == -1) param 
	  				else param.asInstanceOf[Product].productElement(dIdx)
	  			).asInstanceOf[Int]
	      	takeCol.typeMapper(profile).setValue(
	      		math.max(0, takeVal - dropVal), p
	      	)
	  		}
  		case _=> throw new SQueryException(s"""
				values in a take + drop operation cannot be mixed; 
				they must be either ConstColumn[Int] or Param[Int].
				Supplied node $node and Optional compareNode $compareNode
				could not be converted to a literal value
			""")
  	}
  	()
  }
  
}