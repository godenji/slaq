package org.scalaquery.util

trait WithOp extends Cloneable { self: Node =>
  def mapOp(f: Node => Node): this.type = {
    val t = clone
    t._op = f(this)
    t
  }
  private[WithOp] var _op: Node = _
  final def op: Node = _op
  override def clone(): this.type = super.clone.asInstanceOf[this.type]
}