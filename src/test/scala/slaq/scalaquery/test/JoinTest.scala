package slaq.test

import org.junit.Test
import org.junit.Assert._
import slaq.ql._
import slaq.ql.TypeMapper._
import slaq.ql.driver.SQLiteDriver
import slaq.session._
import slaq.test.util._
import slaq.test.util.TestDB._

object JoinTest extends DBTestObject(H2Mem, Postgres, MySQL, HsqldbMem, SQLiteMem)

case class Categories(id: Int, name: String)
object Categories extends Table[Categories]("cats") {
  def id = column[Int]("id", O PrimaryKey)
  def name = column[String]("name")
  def * = id ~ name <> (
    Categories.apply _,
    x => Tuple.fromProductTyped(x) 
  )
}
case class Posts(id: Int, title: String, category: Int, category2: Int)
object Posts extends Table[Posts]("posts") {
  def id = column[Int]("id", O PrimaryKey, O AutoInc)
  def title = column[String]("title")
  def category = column[Int]("category")
  def category2 = column[Int]("category2")
  def catsFk = foreignKey("catsFk", category2, Categories)(_.id)
  def * = id ~ title ~ category ~ category2 <> (
    Posts.apply _,
    x => Tuple.fromProductTyped(x)
  )
}

class JoinTest(tdb: TestDB) extends DBTest(tdb) {
  import tdb.driver.Implicit.{given, *}

  @Test def test(): Unit = db withSession { implicit ss: Session =>

    (Categories.ddl ++ Posts.ddl) create

    Categories.insertAll(
      Categories(1, "Scala"),
      Categories(2, "ScalaQuery"),
      Categories(3, "Windows"),
      Categories(4, "Software")
    )
    (Posts.title ~ Posts.category ~ Posts.category2).insertAll(
      ("Test Post", -1, 1),
      ("Formal Language Processing in Scala, Part 5", 1, 1),
      ("Efficient Parameterized Queries in ScalaQuery", 2, 2),
      ("Removing Libraries and HomeGroup icons from the Windows 7 desktop", 3, 3),
      ("A ScalaQuery Update", 2, 4)
    )

    val q1 = for {
      c <- Categories
      p <- Posts if c.id is p.category
      _ <- Query orderBy p.id
    } yield p.id ~ c.id ~ c.name ~ p.title
    println("Implicit join: " + q1.selectStatement)
    q1.foreach(x => println("  " + x))
    assertEquals(List((2, 1), (3, 2), (4, 3), (5, 2)), q1.map(p => p._1 ~ p._2).list())

    val q2 = for {
      (c, p) <- Categories join Posts on (_.id is _.category)
      _ <- Query orderBy p.id
    } yield p.id ~ c.id ~ c.name ~ p.title
    println("Explicit inner join: " + q2.selectStatement)
    q2.foreach(x => println("  " + x))
    assertEquals(List((2, 1), (3, 2), (4, 3), (5, 2)), q2.map(p => p._1 ~ p._2).list())

    val q3 = for {
      (c, p) <- Categories leftJoin Posts on (_.id is _.category)
      _ <- Query orderBy p.id.nullsFirst
    } yield p.id ~ c.id ~ c.name ~ p.title
    println("Left outer join (nulls first): " + q3.selectStatement)
    q3.foreach(x => println("  " + x))
    assertEquals(List((0, 4), (2, 1), (3, 2), (4, 3), (5, 2)), q3.map(p => p._1 ~ p._2).list())

    val q3b = for {
      (c, p) <- Categories leftJoin Posts on (_.id is _.category)
      _ <- Query orderBy p.id.nullsLast
    } yield p.id ~ c.id ~ c.name ~ p.title
    println("Left outer join (nulls last): " + q3b.selectStatement)
    q3b.foreach(x => println("  " + x))
    assertEquals(List((2, 1), (3, 2), (4, 3), (5, 2), (0, 4)), q3b.map(p => p._1 ~ p._2).list())

    if (tdb.driver != SQLiteDriver) { // SQLite does not support right and full outer joins
      val q4 = for {
        (c, p) <- Categories rightJoin Posts on (_.id is _.category)
        _ <- Query orderBy p.id
      } yield p.id ~ c.id ~ c.name ~ p.title
      println("Right outer join: " + q4.selectStatement)
      q4.foreach(x => println("  " + x))
      assertEquals(List((1, 0), (2, 1), (3, 2), (4, 3), (5, 2)), q4.map(p => p._1 ~ p._2).list())
    }

    val q5 = for {
      (c, p) <- Categories join Posts on (_.id is _.category)
      _ <- Query orderBy p.id
    } yield p.id
    println("Inner join (single selected column): " + q5.selectStatement)
    q5.foreach(x => println("  " + x))
    assertEquals(List(2, 3, 4, 5), q5.list())

    val q6 = for {
      (c, p) <- Categories join Posts on (_.catsFk)
      _ <- Query orderBy p.id
    } yield c
    println("Foreign key join: " + q6.selectStatement)
    q6.foreach(x => println("  " + x))
    assertEquals(Set(1, 2, 3, 4), q6.to[Set]().map(_.id))
  }
}
