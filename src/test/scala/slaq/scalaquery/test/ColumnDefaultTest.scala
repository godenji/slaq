package slaq.test

import org.junit.Test
import org.junit.Assert._
import slaq.ql._
import slaq.ql.TypeMapper._
import slaq.ql.Table
import slaq.session._
import slaq.test.util._
import slaq.test.util.TestDB._

object ColumnDefaultTest extends DBTestObject(H2Mem, SQLiteMem, Postgres, MySQL, HsqldbMem)

class ColumnDefaultTest(tdb: TestDB) extends DBTest(tdb) {
  import tdb.driver.Implicit.{given, *}

  case class User(id: Int, first: String, last: String)

  object A extends Table[(Int, String, Option[Boolean])]("a") {
    def id = column[Int]("id")
    def a = column[String]("a", O.Default("foo"))
    def b = column[Option[Boolean]]("b", O.Default(Some(true)))
    def * = id ~ a ~ b
  }

  @Test def test(): Unit = {
    db withSession { implicit ss: Session =>
      A.ddl.createStatements foreach println
      A.ddl.create
      A.id insert 42
      assertEquals(List((42, "foo", Some(true))), Query(A).list())
    }
  }
}
