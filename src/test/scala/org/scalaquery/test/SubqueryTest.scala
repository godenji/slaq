package org.scalaquery.test

import org.junit.Test
import org.junit.Assert._
import org.scalaquery.ql._
import org.scalaquery.ql.TypeMapper._
//import org.scalaquery.ql.driver.SQLiteDriver
import org.scalaquery.session._
import org.scalaquery.test.util._
import org.scalaquery.test.util.TestDB._

object SubqueryTest extends DBTestObject(H2Mem)

trait SubqueryModel {
  case class Categories(id: Int, name: String)
  object Categories extends Table[Categories]("cats") {
    def id = column[Int]("id", O PrimaryKey)
    def name = column[String]("name")
    def * = id ~ name <> (Categories.apply _, Categories.unapply _)
  }
  case class Posts(id: Int, title: String, category: Int, category2: Int)
  object Posts extends Table[Posts]("posts") {
    def id = column[Int]("id", O PrimaryKey, O AutoInc)
    def title = column[String]("title")
    def category = column[Int]("category")
    def category2 = column[Int]("category2")
    def catsFk = foreignKey("catsFk", category2, Categories)(_.id)
    def * = id ~ title ~ category ~ category2 <> (Posts.apply _, Posts.unapply _)
  }
}

class SubqueryTest(tdb: TestDB) extends DBTest(tdb) with SubqueryModel {
  import tdb.driver.Implicit._

  @Test def test(): Unit = db withSession { implicit ss: Session =>

    (Categories.ddl ++ Posts.ddl) create

    Categories insertAll (
      Categories(1, "Scala"),
      Categories(2, "ScalaQuery"),
      Categories(3, "Windows"),
      Categories(4, "Software")
    )
    Posts.title ~ Posts.category ~ Posts.category2 insertAll (
      ("Test Post", -1, 1),
      ("Formal Language Processing in Scala, Part 5", 1, 1),
      ("Efficient Parameterized Queries in ScalaQuery", 2, 2),
      ("Removing Libraries and HomeGroup icons from the Windows 7 desktop", 3, 3),
      ("A ScalaQuery Update", 2, 4)
    )

    def qbase = for {
      c <- Categories.clone
      p <- Posts.clone leftJoin (_.id is c.id)
    } yield (c, p)

    val q = for {
      (c, p) <- qbase
      if c.id in (
        for {
          (c2, p2) <- qbase
          _ <- Query groupBy (c2.id, p2.id) orderBy p2.id
        } yield c2.id
      )
      _ <- Query groupBy c.id orderBy p.id
    } yield (c, p, (c.id.sum, p.id.sum))
    println(q.pretty)
    q.foreach(x => println("  " + x))

    //    val q = for {
    //      c <- Categories
    //      p <- Posts leftJoin (_.id is c.id)
    //      _ <- Query groupBy c.id orderBy p.id
    //    } yield (c, p, (c.id.sum, p.id.sum))
    //
    //    val sub = for {
    //      (a, b, cnt) <- q.subquery
    //      c <- Categories leftJoin (_.id is a.id)
    //    } yield (a, b, cnt)
    //    println(sub.pretty)
    //    sub.foreach(x => println("  " + x))

    //    val q1 = for {
    //      c <- Categories
    //      p <- Posts if c.id is p.category
    //      _ <- Query orderBy p.id
    //    } yield p.id ~ c.id ~ c.name ~ p.title
    //assertEquals(List((2, 1), (3, 2), (4, 3), (5, 2)), q1.map(p => p._1 ~ p._2).list)
    assertEquals(false, true)

  }
}
