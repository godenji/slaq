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

  trait PostLike {
    def category: Column[Int]
    def categoryJoin: Query[Categories.type, Nothing]
  }
  /* Table definition needs to be a val instead of an object here
   * due to scalac referencing the wrong "this" in `categoryJoin` filter, which then
   * generates an extraneous `posts` table alias.
   *
   * Could probably also remove this test since the functionality here is non-idiomatic,
   * and even if one wanted to use this approach there's a fair bit of boilerplate due to
   * restricted implicit conversion capabilities in Scala 3 vs. Scala 2.
   *
   * Will leave it for now, odd as it is it's yet another way to define table mappings.
   */
  val Posts = new Table[Int]("posts") with PostLike {
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
