package org.scalaquery.test

import org.junit.Test
import org.junit.Assert._
import org.scalaquery.ql._
import org.scalaquery.ql.Table
import org.scalaquery.test.util._
import org.scalaquery.test.util.TestDB._
import org.scalaquery.session.Session

object TransactionTest extends DBTestObject(H2Disk, SQLiteDisk, Postgres, MySQL, HsqldbDisk)

class TransactionTest(tdb: TestDB) extends DBTest(tdb) {
  import tdb.driver.Implicit._

  @Test def test() {

    val T = new Table[Int]("t") {
      def a = column[Int]("a")
      def * = a
    }

    db withSession { implicit ss: Session =>
      T.ddl.create
    }

    val q = Query(T)

    db withTransaction { implicit ss: Session =>
      T.insert(42)
      assertEquals(Some(42), q.firstOption)
      ss.rollback()
    }
    assertEquals(None, db withSession { implicit ss: Session => q.firstOption })

    def bInsert(implicit ss: Session) = {
      T.insert(2)
    }
    db.withTransaction { implicit ss: Session =>
      val res = for {
        a <- Right(T.insert(1)).right
        b <- Right(bInsert).right
      } yield (a, b)
      assertEquals((1, 1), res.right.get)
      ss.rollback()
    }
    assertEquals(None, db withSession { implicit ss: Session => q.firstOption })
  }
}
