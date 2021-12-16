package slaq.test

import org.junit.Test
import org.junit.Assert._
import slaq.ql._
import slaq.ql.Table
import slaq.test.util._
import slaq.test.util.TestDB._
import java.util.UUID
import slaq.session.Session

object UUIDTest extends DBTestObject(H2Mem, SQLiteMem, Postgres, MySQL, HsqldbMem)

class UUIDTest(tdb: TestDB) extends DBTest(tdb) {
  import tdb.driver.Implicit.{given, *}

  object T1 extends Table[(Int, Option[UUID])]("test") {
    def id = column[Int]("id")
    def data = column[Option[UUID]]("data")
    def * = id ~ data
  }

  object T2 extends Table[(Int, UUID)]("test2") {
    def id = column[Int]("id", O PrimaryKey)
    def data = column[UUID]("data")
    def * = id ~ data
  }

  val u1 = Some(java.util.UUID.randomUUID())
  val u2 = None
  val u3 = java.util.UUID.randomUUID()
  val u4 = java.util.UUID.randomUUID()
  val u5 = java.util.UUID.randomUUID()

  @Test def test() = db withSession { implicit ss: Session =>
    (T1.ddl ++ T2.ddl).create;

    T2.insert((1, u3))
    T2.insert((2, u4))
    assertEquals(Set((1, u3), (2, u4)), Query(T2).to[Set]())

    T1 insert ((1, u1))
    T1 insert ((2, u2))
    assertEquals(Set((1, u1), (2, u2)), Query(T1).to[Set]())

    val q2 = for { t <- T2 if t.id =~ 1 } yield t.data
    q2.update(u5)
    assertEquals(Set((1, u5), (2, u4)), Query(T2).to[Set]())
  }
}
