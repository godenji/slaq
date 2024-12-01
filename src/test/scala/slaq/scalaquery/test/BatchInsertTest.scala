package slaq.test

import org.junit.Test
import org.junit.Assert._
import slaq.ql._
import driver.MySQLDriver
import TypeMapper._
import slaq.session._
import slaq.test.util._
import TestDB._

// note: SQLite jdbc driver does not properly support generated keys
object BatchInsertTest extends DBTestObject(H2Mem, /*SQLiteMem,*/ Postgres, MySQL, HsqldbMem)

class BatchInsertTest(tdb: TestDB) extends DBTest(tdb) {
  import tdb.driver.Implicit.{given, *}

  case class User(id: Int, first: String, last: Option[String])

  object User extends Table[User]("users") {
    def id = column[Int]("id", O PrimaryKey, O AutoInc)
    def first = column[String]("first", O DBType "varchar(64)")
    def last = column[Option[String]]("last")
    def * = id ~ first ~ last <> (User.apply, x => Tuple.fromProductTyped(x))
  }

  @Test def insertWithoutPK(): Unit = {
    db withSession { implicit ss: Session =>
      User.ddl.create

      val res = (User.first ~ User.last).insertAll(
        ("Marge", Some("Simpson")), ("Apu", Some("Nahasapeemapetilon")),
        ("Carl", Some("Carlson")), ("Lenny", Some("Leonard"))
      )
      assertEquals(res.generatedKeys, List(1, 2, 3, 4))
    }
  }

  @Test def insertWithPK(): Unit = {
    db withSession { implicit ss: Session =>
      User.ddl.create

      val (one, two, three, four) =
        // mysql generated keys are incorrect when inserting non-zero value
        if (tdb.driver == MySQLDriver) (0, 0, 0, 0)
        else (1, 2, 3, 4)

      val res = User.insertAll(
        User(one, "Marge", Some("Simpson")),
        User(two, "Apu", Some("Nahasapeemapetilon")),
        User(three, "Carl", Some("Carlson")),
        User(four, "Lenny", Some("Leonard"))
      )
      assertEquals(res.generatedKeys, List(1, 2, 3, 4))
    }
  }

  @Test def insertEmpty(): Unit = {
    db withSession { implicit ss: Session =>
      User.ddl.create

      val model = List()
      val res = User.insertAll(model*)
      assertEquals(res.generatedKeys, List.empty)
    }
  }
}
