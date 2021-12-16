package slaq.test

import scala.language.reflectiveCalls
import scala.collection.mutable.ArrayBuffer
import org.junit.Test
import org.junit.Assert._
import slaq.ql._
import slaq.ql.Table
import slaq.test.util._
import slaq.test.util.TestDB._
import slaq.util.CloseableIterator
import slaq.session.Session

object InvokerTest extends DBTestObject(H2Mem)

class InvokerTest(tdb: TestDB) extends DBTest(tdb) {
  import tdb.driver.Implicit.{given, *}

  @Test def testCollections(): Unit = {

    object T extends Table[Int]("t") {
      def a = column[Int]("a")
      def * = a
    }

    db withSession { implicit session =>
      T.ddl.create
      T.insertAll(2, 3, 1, 5, 4)

      val q = for {
        t <- T
        _ <- Query orderBy t.a
      } yield t.a

      val r1 = q.list()
      assertEquals(List(1, 2, 3, 4, 5), r1)

      val r2 = q.to[List]()
      assertEquals(List(1, 2, 3, 4, 5), r2)

      val r3 = q.to[Set]()
      assertEquals(Set(3, 4, 2, 1, 5), r3)

      val r4 = q.to[IndexedSeq]()
      assertEquals(IndexedSeq(1, 2, 3, 4, 5), r4)

      val r5 = q.to[ArrayBuffer]()
      assertEquals(ArrayBuffer(1, 2, 3, 4, 5), r5)

      val r6 = q.to[Array]()
      assertEquals(Array(1, 2, 3, 4, 5).toList, r6.toList)
    }
  }

  @Test def testMap(): Unit = {

    object T extends Table[(Int, String)]("t") {
      def k = column[Int]("k")
      def v = column[String]("v")
      def * = k ~ v
    }

    db withSession { implicit ss: Session =>
      T.ddl.create
      T.insertAll(2 -> "b", 3 -> "c", 1 -> "a")

      val q = Query(T)

      val r1 = q.toMap
      assertEquals(Map(1 -> "a", 2 -> "b", 3 -> "c"), r1)
    }
  }

  @Test def testLazy(): Unit = {

    object T extends Table[Int]("t") {
      def a = column[Int]("a")
      def * = a
    }

    val q = for {
      t <- T
      _ <- Query orderBy t.a
    } yield t

    def setUp(using Session): Unit = {
      T.ddl.create
      for (g <- 1 to 1000 grouped 100)
        T.insertAll(g: _*)
    }

    def f() = CloseableIterator close db.createSession() after { implicit session =>
      setUp
      q.elements()
    }

    def g() = CloseableIterator close db.createSession() after { implicit session =>
      setUp
      throw new Exception("make sure it gets closed")
    }

    val it = f()
    it.use { assertEquals((1 to 1000).toList, it.to(LazyList)) }
    assertFail(g())
    val it2 = f()
    it2.use { assertEquals((1 to 1000).toList, it2.to(LazyList)) }
  }
}
