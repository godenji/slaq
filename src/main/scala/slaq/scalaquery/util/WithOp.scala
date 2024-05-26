package slaq.util

trait WithOp extends Cloneable { self: Node =>
  def mapOp(f: Node => Node): this.type = {
    val t = clone
    t._op = f(this)
    t.asInstanceOf[this.type]
  }
  private[WithOp] var _op: Node = scala.compiletime.uninitialized
  final def op: Node = _op
  override def clone(): this.type = super.clone.asInstanceOf[this.type]
}