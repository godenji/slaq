package org.scalaquery

class Fail(msg: String, parent: Throwable) extends RuntimeException(msg, parent)
object Fail {
	def apply(msg: String, parent: Throwable = null) = throw new Fail(msg, parent)
}