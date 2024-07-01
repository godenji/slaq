package slaq.test

import org.junit.Test
import org.junit.Assert._
import slaq.ql._
import slaq.ql.TypeMapper._
import slaq.ql.Table
import slaq.session._
import slaq.test.util._
import slaq.test.util.TestDB._

object MutateTest extends DBTestObject(H2Mem, Postgres, MySQL, HsqldbMem)

class MutateTest(tdb: TestDB) extends DBTest(tdb) {
  import tdb.driver.Implicit.{given, *}

  case class User(id: Int, first: String, last: String)
  object Users extends Table[User]("users") {
    def id = column[Int]("id", O PrimaryKey, O AutoInc)
    def first = column[String]("first")
    def last = column[String]("last")
    def * = id ~ first ~ last <> (
      User.apply,
      x => Tuple.fromProductTyped(x)
    )
  }

  private def setup()(using Session) =
    Users.ddl.create
    Users.insertAll(
      User(1, "Marge", "Bouvier"),
      User(2, "Homer", "Simpson"),
      User(3, "Bart", "Simpson"),
      User(4, "Carl", "Carlson")
    )

  @Test def mutate(): Unit = {
    db withSession { implicit ss: Session =>
      setup()

      println("Before mutating:")
      Query(Users).foreach(u => println("  " + u))

      val q1 = for (u <- Users if u.last =~ "Simpson".bind | u.last =~ "Bouvier".bind) yield u
      q1.mutate { m =>
        println("***** Row: " + m.row)
        if (m.row.last == "Bouvier") m.row = m.row.copy(last = "Simpson")
        else if (m.row.first == "Homer") m.delete()
        else if (m.row.first == "Bart") m.insert(User(42, "Lisa", "Simpson"))
      }

      println("After mutating:")
      Query(Users).foreach(u => println("  " + u))

      assertEquals(
        Set("Marge Simpson", "Bart Simpson", "Lisa Simpson", "Carl Carlson"),
        (for (u <- Users) yield u.first ++ " " ++ u.last).list().toSet
      )
    }
  }

  @Test def lockFreeMutate(): Unit = {
    val byId = Users.createFinderBy(_.id)
    val queryTemplate = byId(1)
    val updatedUser = User(1, "Marge", "Simpson")

    db withSession { implicit ss: Session =>
      setup()

      println("update statement")
      println(queryTemplate.updateStatement)

      queryTemplate.update(updatedUser)
      assertEquals(Some(updatedUser), queryTemplate.firstOption)

      println("delete statement")
      println(queryTemplate.deleteStatement)

      queryTemplate.delete
      assertEquals(None, queryTemplate.firstOption)
    }
  }
}
