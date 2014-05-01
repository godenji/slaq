package org.scalaquery.ql

import org.scalaquery.util.{Node, ValueLinearizer}

/**
 * A packed value together with its unpacking
 */
case class Unpackable[T, U](value: T, unpack: Unpack[T, U]) {
  def endoMap(f: T => T): Unpackable[T, U] = new Unpackable(f(value), unpack)
  def reifiedNode = Node(unpack.reify(value))
  def reifiedUnpackable[R](implicit ev: Reify[T, R]): Unpackable[R, U] = Unpackable(unpack.reify(value).asInstanceOf[R], unpack.reifiedUnpack.asInstanceOf[Unpack[R, U]])
  def linearizer = unpack.linearizer(value).asInstanceOf[ValueLinearizer[U]]
  def mapOp(f: Node => Node) = unpack.mapOp(value, f).asInstanceOf[T]
}
