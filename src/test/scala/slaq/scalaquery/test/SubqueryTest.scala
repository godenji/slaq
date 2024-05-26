package slaq.test

import org.junit.Test
import org.junit.Assert._
import slaq.ql._
import slaq.ql.TypeMapper._
import slaq.session._
import slaq.test.util._
import slaq.test.util.TestDB._

object SubqueryTest extends DBTestObject(H2Mem, SQLiteMem)

trait SubqueryModel {
  case class Categories(id: Int, name: String)
  object Categories extends Table[Categories]("cats") {
    def id = column[Int]("id", O PrimaryKey)
    def name = column[String]("name")
    def * = id ~ name <> (
      Categories.apply,
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
      Posts.apply,
      x => Tuple.fromProductTyped(x)
    )
  }
}

class SubqueryTest(tdb: TestDB) extends DBTest(tdb) with SubqueryModel {
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

    val sqJoin = for
      (max, c, p, p2) <- (
        for
          (c, p) <- Categories join Posts on (_.id is _.id)
          p2 <- Posts leftJoin (_.id is p.id)
          _ <- Query groupBy c.id orderBy p.id
        yield (c.id.max, c, p, p2)
      ).subquery if max =~ ConstColumn(4) & c.id =~ p.id
    yield (max, c, p, p2)
    println(sqJoin.pretty)
    sqJoin.foreach(x => println("  " + x))
    assertEquals(
      Some(4),
      sqJoin.list().headOption.flatMap(_._1)
    )

    val sqCross = for
      (max, c, p) <- (
        for
          c <- Categories
          p <- Posts if p.id is c.id
          _ <- Query groupBy c.id orderBy p.id
        yield (c.id.max, c, p)
      ).subquery if max =~ ConstColumn(4)
    yield (max, c, p)
    println(sqCross.pretty)
    sqCross.foreach(x => println("  " + x))
    assertEquals(
      Some(4),
      sqCross.list().headOption.flatMap(_._1)
    )

    def qbase = for {
      c <- Categories.clone
      p <- Posts.clone leftJoin (_.id is c.id)
    } yield (c, p)

    val whereIn = for {
      (c, p) <- qbase
      if c.id in (
        for {
          (c2, p2) <- qbase
          _ <- Query groupBy (c2.id, p2.id) orderBy p2.id
        } yield c2.id
      )
      _ <- Query groupBy c.id orderBy p.id
    } yield (c, p, (c.id.sum, p.id.sum))
    println(whereIn.pretty)
    whereIn.foreach(x => println("  " + x))
    assertEquals(
      Some((Some(4), Some(4))),
      whereIn.list().reverse.headOption.map(_._3)
    )
  }
}
