package slaq.test

import scala.language.reflectiveCalls
import org.junit.Test
import org.junit.Assert._
import slaq.ql._
import slaq.ql.TypeMapper._
import slaq.ql.driver.H2Driver.Implicit.{given, *}
import slaq.ql.Table

object TablesInObjectTest {
  def main(args: Array[String]): Unit = { new TablesInObjectTest().test1() }

  object Categories extends Table[Int]("categories") {
    def id = column[Int]("id")
    def * = id
  }

  /* This needs to be a val instead of an object with a singleton type
   * because scalac assumes that the object is a singleton and pulls the
   * wrong "this" reference into the closure -- where "category" is
   * referenced -- when it is used in a clone()d Posts instance.
   */
  object Posts extends Table[Int]("posts") {
    def category = column[Int]("category")
    def * = category
    def categoryJoin = Categories.filter(_.id =~ category)
  }
}

class TablesInObjectTest {
  import TablesInObjectTest._

  @Test def test1() = {

    def categoryJoin(p: Posts.type) = Categories.filter(_.id =~ p.category)

    val q1 = for {
      p <- Posts
      c <- categoryJoin(p.asInstanceOf[Posts.type])
    } yield p.category ~ c.id
    //q1.dump("Local function")
    val sel1 = q1.selectStatement
    println("Local function:  " + sel1)

    val q2 = for {
      p <- Posts
      c <- p.categoryJoin
    } yield p.category ~ c.id
    //q1.dump("Method on table")
    val sel2 = q2.selectStatement
    println("Method on table: " + sel2)

    assertEquals(sel1, sel2)
  }
}
