package org.scalaquery.ql

sealed abstract class JoinType(val sqlName: String)
object JoinType{
	case object Inner extends JoinType("INNER")
	case object Left  extends JoinType("LEFT OUTER")
	case object Right extends JoinType("RIGHT OUTER")
	case object Outer extends JoinType("FULL OUTER")
}