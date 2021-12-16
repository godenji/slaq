package slaq.test

import org.junit.Test
import org.junit.Assert._
import slaq.ql._
import slaq.ql.TypeMapper._
import slaq.ql.Table
import slaq.session._
import slaq.test.util._
import slaq.test.util.TestDB._

object MapperTest extends DBTestObject(H2Mem, SQLiteMem, Postgres, MySQL, HsqldbMem)

class MapperTest(tdb: TestDB) extends DBTest(tdb) {
  import tdb.driver.Implicit.{given, *}

  @Test def testMappedEntity(): Unit = {

    case class User(id: Option[Int], first: String, last: String)

    object Users extends Table[User]("users") {
      def id = column[Int]("id", O PrimaryKey, O AutoInc)
      def first = column[String]("first")
      def last = column[String]("last")
      def * = id.? ~ first ~ last <> (
        User.apply _,
        x => Tuple.fromProductTyped(x)
      )
      def forInsert = first ~ last <>
        ({ (f, l) => User(None, f, l) }, { (u: User) => (u.first, u.last) })
      val findByID = this.createFinderBy(_.id)
    }

    db withSession { implicit ss: Session =>

      Users.ddl.create
      (Users.first ~ Users.last).insert(("Homer", "Simpson"))
      /* Using Users.forInsert so that we don't put a NULL value into the ID
       * column. H2 and SQLite allow this but PostgreSQL doesn't. */
      Users.forInsert.insertAll(
        User(None, "Marge", "Bouvier"),
        User(None, "Carl", "Carlson"),
        User(None, "Lenny", "Leonard")
      )

      val updateQ = Users.filter(_.id =~ 2.bind).map(_.forInsert)
      println("Update: " + updateQ.updateStatement)
      updateQ.update(User(None, "Marge", "Simpson"))

      Users.filter(_.id.between(1, 2)).foreach(println)
      println("ID 3 -> " + Users.findByID.first(3))

      assertEquals(
        Set(User(Some(1), "Homer", "Simpson"), User(Some(2), "Marge", "Simpson")),
        Users.filter(_.id.between(1, 2)).list().toSet
      )
      assertEquals(
        User(Some(3), "Carl", "Carlson"),
        Users.findByID.first(3)
      )
    }
  }

  @Test def testUpdate(): Unit = {

    case class Data(a: Int, b: Int)

    object Ts extends Table[Data]("T") {
      def a = column[Int]("A")
      def b = column[Int]("B")
      def * = a ~ b <> (
        Data.apply _,
        x => Tuple.fromProductTyped(x)  
      )
    }

    db withSession { implicit ss: Session =>
      Ts.ddl.create
      Ts.insertAll(new Data(1, 2), new Data(3, 4), new Data(5, 6))

      val updateQ = Ts.filter(_.a =~ 1)
      updateQ.dump("updateQ: ")
      println("Update: " + updateQ.updateStatement)
      updateQ.update(Data(7, 8))

      assertEquals(
        Set(Data(7, 8), Data(3, 4), Data(5, 6)),
        Query(Ts).list().toSet
      )
    }
  }

  @Test def testMappedType(): Unit = {

    enum Bool:
      case True extends Bool
      case False extends Bool
    import Bool.*

    given BaseTypeMapper[Bool] = MappedTypeMapper.base[Bool, Int](
      b => if (b == True) 1 else 0,
      i => if (i == 1) True else False
    )

    object T extends Table[(Int, Bool)]("t") {
      def id = column[Int]("id", O PrimaryKey, O AutoInc)
      def b = column[Bool]("b")
      def * = id ~ b
    }

    db withSession { implicit ss: Session =>
      T.ddl.create
      T.b.insertAll(False, True)
      assertEquals(Query(T).list().toSet, Set((1, False), (2, True)))
      assertEquals(T.filter(_.b =~ (True: Bool)).list().toSet, Set((2, True)))
      assertEquals(T.filter(_.b =~ (False: Bool)).list().toSet, Set((1, False)))
    }
  }
}
