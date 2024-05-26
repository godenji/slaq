package slaq.test

import org.junit.Test
import org.junit.Assert._
import slaq.ql._
import slaq.ql.TypeMapper._
import slaq.ql.Table
import slaq.session._
import slaq.test.util._
import slaq.test.util.TestDB._

object PagingTest extends DBTestObject(
  H2Mem, SQLiteMem, Postgres, MySQL, HsqldbMem
)

class PagingTest(tdb: TestDB) extends DBTest(tdb) {
  import tdb.driver.Implicit.{given, *}

  object IDs extends Table[Int]("ids") {
    def id = column[Int]("id", O PrimaryKey)
    def * = id
  }

  @Test def test(): Unit = {
    db withSession { implicit ss: Session =>

      IDs.ddl.create;
      IDs.insertAll((1 to 10)*)

      val q1 = for (i <- IDs; _ <- Query orderBy i.id) yield i
      println("q1: " + q1.selectStatement)
      println("    " + q1.list())
      assertEquals(1 to 10 toList, q1.list())

      val q2 = q1 take 5
      println("q2: " + q2.selectStatement)
      println("    " + q2.list())
      assertEquals(1 to 5 toList, q2.list())

      val q3 = q1 drop 5
      println("q3: " + q3.selectStatement)
      println("    " + q3.list())
      assertEquals(6 to 10 toList, q3.list())

      val q4 = q1 drop 5 take 3
      println("q4: " + q4.selectStatement)
      println("    " + q4.list())
      assertEquals(6 to 8 toList, q4.list())

      // test cacheable Params paginate
      val q5 = for {
        fetch ~ offset <- Params[Int, Int]
        x <- q1 take fetch drop offset
      } yield x
      println("q5: " + q5.selectStatement)
      println("    " + q5((5, 3)).list())
      assertEquals(4 to 5 toList, q5((5, 3)).list())

      val q6 = q1 take 0
      println("q6: " + q6.selectStatement)
      println("    " + q6.list())
      assertEquals(List(), q6.list())

    }
  }
}
