package org.scalaquery

package object test {
  def echo(x: String) = System.console.printf(x + "\n")
  implicit class AnyProvider(x: Any) {
    def echo: Unit = System.console.printf(x + "\n")
  }
}