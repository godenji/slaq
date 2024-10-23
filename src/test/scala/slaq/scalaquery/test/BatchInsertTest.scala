package slaq.test

import org.junit.Test
import org.junit.Assert._
import slaq.ql._
import TypeMapper._
import slaq.session._
import slaq.test.util._
import TestDB._

// note: SQLite jdbc driver does not properly support generated keys
object BatchInsertTest extends DBTestObject(H2Mem, /*SQLiteMem,*/ Postgres, MySQL, HsqldbMem)

class BatchInsertTest(tdb: TestDB) extends DBTest(tdb) {
  import tdb.driver.Implicit.{given, *}

  case class User(id: Int, first: String, last: String)

  object Users extends Table[(Int, String, Option[String])]("users") {
    def id = column[Int]("id", O PrimaryKey, O AutoInc)
    def first = column[String]("first", O DBType "varchar(64)")
    def last = column[Option[String]]("last")
    def * = id ~ first ~ last
  }

  @Test def test(): Unit = {
    db withSession { implicit ss: Session =>

      val ddl = Users.ddl
      ddl.create
      val ins = (Users.first ~ Users.last).insertAll(
        ("Marge", Some("Simpson")), ("Apu", Some("Nahasapeemapetilon")),
        ("Carl", Some("Carlson")), ("Lenny", Some("Leonard"))
      )(withGeneratedKeys = true)
      println(s"Generated keys ${ins.generatedKeys}")
      assert(ins.generatedKeys == List(1, 2, 3, 4))
    }
  }
}
