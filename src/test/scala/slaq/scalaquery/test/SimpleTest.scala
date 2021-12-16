package slaq.test

import org.junit.Test
import org.junit.Assert._
import slaq.simple.{GetResult, StaticQuery => Q}
import slaq.test.util._
import slaq.test.util.TestDB._
import slaq.session.Session

object SimpleTest extends DBTestObject(H2Mem, H2Disk, SQLiteMem, SQLiteDisk, Postgres, MySQL, HsqldbMem)

class SimpleTest(tdb: TestDB) extends DBTest(tdb) {

  given GetResult[User] = GetResult(r => new User(r<<, r<<))

  case class User(id: Int, name: String)

  @Test def test(): Unit = {
    def getUsers(id: Option[Int]) = {
      val q = Q[User] + "select id, name from users "
      id map { q + "where id =" +? _ } getOrElse q
    }

    def InsertUser(id: Int, name: String) = Q.u + "insert into users values (" +? id + "," +? name + ")"

    val createTable = Q[Int] + "create table users(ID int not null primary key, NAME varchar(255))"
    val populateUsers = List(InsertUser(1, "szeiger"), InsertUser(0, "admin"), InsertUser(2, "guest"), InsertUser(3, "foo"))

    val allIDs = Q[Int] + "select id from users"
    val userForID = Q[Int, User] + "select id, name from users where id = ?"
    val userForIdAndName = Q[(Int, String), User] + "select id, name from users where id = ? and name = ?"

    db withTransaction { implicit ss: Session =>
      println("Creating user table: " + createTable.first())
      println("Inserting users:")
      for (i <- populateUsers) println("  " + i.first())

      println("All IDs:")
      for (s <- allIDs.list()) println("  " + s)
      assertEquals(Set(1, 0, 2, 3), allIDs.list().toSet)

      println("All IDs with foreach:")
      var s1 = Set[Int]()
      allIDs foreach { s =>
        println("  " + s)
        s1 += s
      }
      assertEquals(Set(1, 0, 2, 3), s1)

      val res = userForID.first(2)
      println("User for ID 2: " + res)
      assertEquals(User(2, "guest"), res)

      assertEquals(User(2, "guest"), userForIdAndName((2, "guest")).first())
      assertEquals(None, userForIdAndName((2, "foo")).firstOption)

      println("User 2 with foreach:")
      var s2 = Set[User]()
      userForID(2) foreach { s =>
        println("  " + s)
        s2 += s
      }
      assertEquals(Set(User(2, "guest")), s2)

      println("User 2 with foreach:")
      var s3 = Set[User]()
      getUsers(Some(2)) foreach { s =>
        println("  " + s)
        s3 += s
      }
      assertEquals(Set(User(2, "guest")), s3)

      println("All users with foreach:")
      var s4 = Set[User]()
      getUsers(None) foreach { s =>
        println("  " + s)
        s4 += s
      }
      assertEquals(Set(User(1, "szeiger"), User(2, "guest"), User(0, "admin"), User(3, "foo")), s4)

      println("All users with elements.foreach:")
      var s5 = Set[User]()
      for (s <- getUsers(None).elements()) {
        println("  " + s)
        s5 += s
      }
      assertEquals(Set(User(1, "szeiger"), User(2, "guest"), User(0, "admin"), User(3, "foo")), s5)

      if (tdb.canGetLocalTables) {
        println("All tables:")
        for (t <- tdb.getLocalTables) println("  " + t)
        assertEquals(List("users"), tdb.getLocalTables.map(_.toLowerCase))
      }
      tdb.assertUnquotedTablesExist("users")
    }
  }
}
