package org.scalaquery

package object test {
  def echo(x: String) = System.console.printf(s"${x}\n")
  implicit class AnyProvider(x: Any) {
    def echo: Unit = System.console.printf(s"${x}\n")
  }
}
