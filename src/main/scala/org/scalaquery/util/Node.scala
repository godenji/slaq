package org.scalaquery.util

import java.io.{PrintWriter, OutputStreamWriter}
import org.scalaquery.Fail
import org.scalaquery.ql.ConstColumn

/**
 * A node in the query AST
 */
trait Node {
  def nodeChildren: List[Node]
  def nodeDelegate: Node = this
  def isNamedTable = false

  def nodeNamedChildren: Seq[(Node, String)] = 
  	nodeChildren.toStream.zip(Stream.from(0).map(_.toString))

  def dump(dc: Node.DumpContext, prefix: String, name: String): Unit = {
    val (alias, isFresh) = 
    	if(!isNamedTable) ("", true) else {
	      val name = dc.nc.aliasOrFresh(this)
	      (s"<${name.alias}> ", name.isFresh)
	    }
    dc.out.println(
    	s"$prefix$name$alias${if(isFresh) this else "..."}"
    )
    if(isFresh)
    	nodeNamedChildren.foreach{case(ch, n) =>
    		ch.dump(dc, s"$prefix ", s"$n: ")
    	}
  }

  final def dump(name: String, nc: NamingContext = NamingContext()): Unit = {
    val out = new PrintWriter(new OutputStreamWriter(System.out))
    dump(new Node.DumpContext(out, nc), "", name)
    out.flush()
  }
}

object Node {
  def apply(o: Any): Node = o match {
    case null 			=> ConstColumn.NULL
    case n: Node    => n.nodeDelegate
    case p: Product => new ProductNode { val product = p }
    case r: AnyRef  => fail(s"$o of type ${SimpleTypeName.forVal(r)}")
    case _ => fail(s"$o")
  }
  private def fail(msg: String) = Fail(s"Cannot narrow $msg to Node")

  final class DumpContext(val out: PrintWriter, val nc: NamingContext)
}

trait ProductNode extends Node {
  val product: Product
  lazy val nodeChildren = (
  	for(i <- 0 until product.productArity) yield Node(
  		product.productElement(i)
  	)
  ).toList

  override def toString = s"ProductNode $product"
}

trait BinaryNode extends Node {
  val left: Node
  val right: Node
  def nodeChildren = left :: right :: Nil
}

trait UnaryNode extends Node {
  val child: Node
  def nodeChildren = child :: Nil
}

trait NullaryNode extends Node {
  def nodeChildren = Nil
}
