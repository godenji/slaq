package org.scalaquery.ql

import org.scalaquery.util.{BinaryNode, Node}

final case class JoinPart
	(left: Node, right: Node) extends BinaryNode {
	
  override def toString = "JoinPart"
  override def nodeNamedChildren = 
  	(left, "table") :: (right, "from") :: Nil
}