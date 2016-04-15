package org.scalaquery.test

import org.junit.Test
import org.junit.Assert._
import org.scalaquery.ql._
import org.scalaquery.ql.TypeMapper._
import org.scalaquery.ql.Table
import org.scalaquery.test.util._
import org.scalaquery.test.util.TestDB._
import org.scalaquery.session.Session

object CountTest extends DBTestObject(H2Mem, SQLiteMem, Postgres, MySQL, HsqldbMem, SQLServer)

class CountTest(tdb: TestDB) extends DBTest(tdb) {
  import tdb.driver.Implicit._

  @Test def test() = db withSession { implicit ss:Session=>
    object TestTable extends Table[Int]("TEST") {
      def id = column[Int]("ID")
      def * = id
    }
    TestTable.ddl.create
    TestTable.insertAll(1, 2, 3, 4, 5)

    val q1 = Query(TestTable.id.count)
    q1.dump("q1: ")
    println("q1: "+q1.selectStatement)
    assertEquals(5, q1.first)
    
//    val q2 = for{
//    	t1 <- TestTable
//    	if t1.id is (TestTable.filter(_.id > 1).map(_.id).asColumn)
//    } yield t1
//    println("q2: "+q2.selectStatement)
//    assert(false)
  }
}
