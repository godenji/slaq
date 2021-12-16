package slaq.example

import scala.language.postfixOps
import slaq.ql.{Query, Table}
import slaq.ql.driver.{Driver}, Driver.Implicit.{given, *}
import slaq.ql.TypeMapper._
import slaq.util.NamingContext

object Benchmark {

  val COUNT = 20000
  val PRE_COUNT = 2000

  def main(args: Array[String]): Unit = {
    for (i <- 0 to COUNT) test1(i == 0)
    val t0 = System.nanoTime()
    for (i <- 0 to COUNT) test1(false)
    val t1 = System.nanoTime()
    val total = (t1 - t0) / 1000000.0
    println(s"$COUNT runs tooks $total ms (${total * 1000.0 / COUNT} µs per run)")
  }

  object Users extends Table[(Int, String, String)]("users") {
    def id = column[Int]("id")
    def first = column[String]("first")
    def last = column[String]("last")
    def * = id ~ first ~ last
  }

  object Orders extends Table[(Int, Int)]("orders") {
    def userID = column[Int]("userID")
    def orderID = column[Int]("orderID")
    def * = userID ~ orderID
  }

  def test1(print: Boolean): Unit = {
    val q1 = for (u <- Users) yield u
    val q2 = for {
      u <- Users
      o <- Orders filter { o => (u.id is o.userID) & (u.first isNotNull) }
    } yield u.first ~ u.last ~ o.orderID
    val q3 = for (u <- Users filter (_.id is 42)) yield u.first ~ u.last
    val q4 = for {
      uo <- Users join Orders on (_.id is _.userID)
      (u, o) = uo
      _ <- Query.orderBy(u.last asc)
    } yield u.first ~ o.orderID
    val q5 = for (
      o <- Orders filter { o =>
        o.orderID is (
          for { o2 <- Orders filter (o.userID is _.userID) } yield o2.orderID.max
        ).asColumn
      }
    ) yield o.orderID

    val s1 = Driver.buildSelect(q1, NamingContext())
    val s2 = Driver.buildSelect(q2, NamingContext())
    val s3 = Driver.buildSelect(q3, NamingContext())
    val s4 = Driver.buildSelect(q4, NamingContext())
    val s5 = Driver.buildSelect(q5, NamingContext())

    if (print) {
      println("q1: " + s1)
      println("q2: " + s2)
      println("q3: " + s3)
      println("q4: " + s4)
      println("q5: " + s5)
    }
  }
}
