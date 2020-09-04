package slaq.ql

import slaq.util.{Node, ValueLinearizer}

/**
 * A packed value together with its unpacking
 */
case class Unpackable[T, U](value: T, unpack: Unpack[T, U]) {
  def linearizer = unpack.linearizer(value).asInstanceOf[ValueLinearizer[U]]
  def mapOp(f: Node => Node) = unpack.mapOp(value, f).asInstanceOf[T]

  def reifiedNode = Node(unpack.reify(value))
  def reifiedUnpackable[R](implicit ev: Reify[T, R]): Unpackable[R, U] =
    Unpackable(
      unpack.reify(value).asInstanceOf[R],
      unpack.reifiedUnpack.asInstanceOf[Unpack[R, U]]
    )

  def zip[T2, U2](s2: Unpackable[T2, U2]) =
    Unpackable[(T, T2), (U, U2)](
      (value, s2.value), Unpack.unpackTuple2(unpack, s2.unpack)
    )
}
