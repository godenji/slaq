package slaq.test

import org.junit.Test
import org.junit.Assert._
import slaq.ql._
import slaq.ql.TypeMapper._
import slaq.util.BatchResult
import slaq.session._
import slaq.ql.driver.{MySQLDriver, SQLiteDriver}
import slaq.test.util._
import slaq.test.util.TestDB._

object UpsertTest extends DBTestObject(H2Mem, /*SQLiteMem, */HsqldbMem, Postgres, MySQL)

class UpsertTest(tdb: TestDB) extends DBTest(tdb) {
  import tdb.driver.Implicit.{given, *}

  case class A(id: Int, name: String)

  object A extends Table[A]("a") {
    val colOpts =
      // MySQL requires primary key to be defined inline with AutoInc column
      if tdb.driver == MySQLDriver then Seq(O.PrimaryKey, O.AutoInc)
      else Seq(O.AutoInc)

    def id = column[Int]("id", colOpts*)
    def name = column[String]("name")

    // SQLite automatically adds PK for AutoInc column; for MySQL see above `colOpts`
    def pk =
      if Set(MySQLDriver, SQLiteDriver).contains(tdb.driver) then null
      else primaryKey("pk_id", id)

    def * = (id ~ name) <> (A.apply, x => Tuple.fromProductTyped(x))
  }

  @Test def upsertAdd(): Unit = db withSession { implicit ss: Session =>
    A.ddl.createStatements foreach println
    A.ddl create

    val model = List(A(1, "one"), A(2, "two"))
    A.insertAll(model*)
    A.upsert(A(3, "three"))

    assertEquals(
      Query(A).list().find(_.id == 3),
      Some(A(3, "three"))
    )
  }

  @Test def upsertUpdate(): Unit = db withSession { implicit ss: Session =>
    A.ddl create

    val model = List(A(1, "one"), A(2, "two"), A(3, "three"))
    A.insertAll(model*)
    A.upsert(A(2, "two-updated"))

    val res = Query(A).list()
    assertEquals(res.find(_.id == 2).map(_.name), Some("two-updated"))
  }

  @Test def upsertAddMany(): Unit = db withSession { implicit ss: Session =>
    A.ddl create

    val model = List(A(1, "one"), A(2, "two"))
    A.insertAll(model*)
    A.upsertAll(A(3, "three"), A(4, "four"))

    val res = Query(A).list()
    assertEquals(
      res.find(_.id == 4),
      Some(A(4, "four"))
    )
    assertEquals(res.size, 4)
  }

  @Test def upsertUpdateMany(): Unit = db withSession { implicit ss: Session =>
    A.ddl create

    val model = List(A(1, "one"), A(2, "two"))
    A.insertAll(model*)
    A.upsertAll(A(1, "one-updated"), A(2, "two-updated"))

    val res = Query(A).list()
    assertEquals(
      res.find(_.id == 1),
      Some(A(1, "one-updated"))
    )
    assertEquals(res.size, 2)
  }

  @Test def upsertAddAndUpdateMany(): Unit = db withSession { implicit ss: Session =>
    A.ddl create

    val model = List(A(1, "one"), A(2, "two"))
    A.insertAll(model*)
    A.upsertAll(A(1, "three"), A(4, "four"), A(3, "three-updated"))

    val res = Query(A).list()
    assertEquals(
      res.find(_.id == 4),
      Some(A(4, "four"))
    )
    assertEquals(res.size, 4)
  }

  @Test def upsertEmpty(): Unit = db withSession { implicit ss: Session =>
    A.ddl create

    val model = List()
    assertEquals(A.insertAll(model*), BatchResult())
  }
}
