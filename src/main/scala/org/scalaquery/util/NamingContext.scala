package org.scalaquery.util

import scala.collection.mutable.HashMap
import org.scalaquery.ql._

trait NamingContext {self=>
  def nameFor(t: Node) = checkNameFor(t)._1

  def checkNameFor(t: Node): (String, Boolean)

  def overrideName(node: Node, newName: String): NamingContext = 
  	new NamingContext {
    	def checkNameFor(t: Node) = 
    		if(t eq node) (newName, false) else 
    		self.checkNameFor(t)
  	}
}

object NamingContext {
  def apply() = new NamingContext {
    private val tnames = new HashMap[RefId[Node], String]
    private var nextTid = 1

    private def checkNode(t: Node) = tnames.get(RefId(t)) match{
      case Some(n) => (n,false)
      case None =>
        val n = "t" + nextTid
        //println(s"NamingContext() >> no alias for $t creating $n")
        nextTid += 1
        tnames.put(RefId(t), n)
        (n,true)
    }
    private def matchJoin(n: Node) = n match{
    	case t: Table[_]=> Some(checkNode(t))
    	case _=> None
    }
    def checkNameFor(t: Node) = t match{
  		/*
  		 * TODO: join must have a Table on left or right side so
  		 * .get is (somewhat) safe here, but safer approach exists
  		 */
  		case j:Join[_,_]=> (
  			matchJoin(j.leftNode) orElse matchJoin(j.rightNode)
  		).get
  		/* 
  		 * join part left Node = concrete Table
  		 * join part right node = Join[lTable, rTable] 
  		 */
  		case JoinPart(left,_)=> checkNode(left)
  		/*
  		 * non-join table or select clause column
  		 */
  		case _=> checkNode(t)
    }
    
  }
}
