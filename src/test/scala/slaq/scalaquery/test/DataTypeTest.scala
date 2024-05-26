package slaq.test

import org.junit.Test
import org.junit.Assert._
import slaq.ql._
import slaq.ql.Table
import slaq.test.util._
import slaq.test.util.TestDB._
import slaq.session.Session

object DataTypeTest extends DBTestObject(H2Mem, SQLiteMem, HsqldbMem, MySQL, Postgres)

class DataTypeTest(tdb: TestDB) extends DBTest(tdb) {
  import tdb.driver.Implicit.{given, *}

  @Test def testByteArray(): Unit = {
    object T extends Table[(Int, Array[Byte])]("test") {
      def id = column[Int]("id")
      def data = column[Array[Byte]]("data")
      def * = id ~ data
    }

    db withSession { implicit ss: Session =>
      T.ddl.createStatements foreach println
      T.ddl.create;
      T insert ((1, Array[Byte](1, 2, 3)))
      T insert ((2, Array[Byte](4, 5)))
      assertEquals(Set((1, "123"), (2, "45")), Query(T).list().map { case (id, data) => (id, data.mkString) }.toSet)
    }
  }

  @Test def testNumeric() = db withSession { implicit ss: Session =>
    object T extends Table[(Int, Int, Long, Short, Byte)]("test") {
      def id = column[Int]("id")
      def intData = column[Int]("int_data")
      def longData = column[Long]("long_data")
      def shortData = column[Short]("short_data")
      def byteData = column[Byte]("byte_data")
      def * = id ~ intData ~ longData ~ shortData ~ byteData
    }

    T.ddl.createStatements foreach println
    T.ddl.create;

    def test(data: List[(Int, Int, Long, Short, Byte)]): Unit = {
      T.insertAll(data*)
      val q = for { t <- T; _ <- Query orderBy t.id } yield t
      assertEquals(data, q.list())
      Query(T).delete
    }

    test(List(
      (2, -1, -1L, -1: Short, -1: Byte),
      (3, 0, 0L, 0: Short, 0: Byte),
      (4, 1, 1L, 1: Short, 1: Byte)
    ))

    test(List(
      (1, Int.MinValue, 0L, Short.MinValue, Byte.MinValue),
      (5, Int.MaxValue, 0L, Short.MaxValue, Byte.MaxValue)
    ))

    test(List(
      (1, 0, Long.MinValue, 0, 0),
      (5, 0, Long.MaxValue, 0, 0)
    ))
  }
}
