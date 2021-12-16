package slaq.test

import org.junit.Test
import org.junit.Assert._
import slaq.ql._
import slaq.ql.Table
import slaq.test.util._
import slaq.test.util.TestDB._
import slaq.session.Session

object TransactionTest extends DBTestObject(H2Disk, SQLiteDisk, Postgres, MySQL, HsqldbDisk)

class TransactionTest(tdb: TestDB) extends DBTest(tdb) {
  import tdb.driver.Implicit.{given, *}

  @Test def test(): Unit = {

    object T extends Table[Int]("t") {
      def a = column[Int]("a")
      def * = a
    }

    db withSession { implicit session =>
      T.ddl.create
    }

    val q = Query(T)

    db withTransaction { implicit session =>
      T.insert(42)
      assertEquals(Some(42), q.firstOption)
      session.rollback()
    }
    assertEquals(None, db withSession { implicit session => q.firstOption })

    def bInsert(using Session) = {
      T.insert(2)
    }
    db.withTransaction { implicit ss: Session =>
      val res = for {
        a <- Right(T.insert(1))
        b <- Right(bInsert)
      } yield (a, b)
      assertEquals(Right((1, 1)), res)
      ss.rollback()
    }
    assertEquals(None, db withSession { implicit session => q.firstOption })
  }
}
