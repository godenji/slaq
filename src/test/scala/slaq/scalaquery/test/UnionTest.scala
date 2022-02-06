package slaq.test

import org.junit.Test
import org.junit.Assert._
import slaq.ql._
import slaq.ql.TypeMapper._
import slaq.ql.Table
import slaq.session._
import slaq.test.util._
import slaq.test.util.TestDB._

object UnionTest extends DBTestObject(H2Mem, SQLiteMem, Postgres, MySQL/*, HsqldbMem*/)

class UnionTest(tdb: TestDB) extends DBTest(tdb) {
  import tdb.driver.Implicit.{given, *}

  object Managers extends Table[(Int, String, String)]("managers") {
    def id = column[Int]("id")
    def name = column[String]("name")
    def department = column[String]("department")
    def * = id ~ name ~ department
  }

  object Employees extends Table[(Int, String, Int)]("employees") {
    def id = column[Int]("id")
    def name = column[String]("name")
    def manager = column[Int]("manager")
    def * = id ~ name ~ manager

    // A convenience method for selecting employees by department
    def departmentIs(dept: String) = manager in Managers.filter(_.department is dept).map(_.id)
  }

  @Test def test(): Unit = {
    db withSession { implicit ss: Session =>

      (Managers.ddl ++ Employees.ddl) create

      Managers.insertAll(
        (1, "Peter", "HR"),
        (2, "Amy", "IT"),
        (3, "Steve", "IT")
      )

      Employees.insertAll(
        (4, "Jennifer", 1),
        (5, "Tom", 1),
        (6, "Leonard", 2),
        (7, "Ben", 2),
        (8, "Greg", 3)
      )

      val q1 = for (m <- Managers filter { _.department is "IT" }) yield (m.id, m.name)
      println("Managers in IT: " + q1.selectStatement)
      q1.foreach(o => println("  " + o))

      val q2 = for (e <- Employees filter { _.departmentIs("IT") }) yield (e.id, e.name)
      println("Employees in IT: " + q2.selectStatement)
      q2.foreach(o => println("  " + o))

      val q3 = for (x @ (id, name) <- q1 union q2; _ <- Query.orderBy(name asc)) yield x
      q3.dump("q3: ")
      println()
      println("Combined and sorted: " + q3.selectStatement)
      q3.foreach(o => println("  " + o))

      assertEquals(q3.list(), List((2, "Amy"), (7, "Ben"), (8, "Greg"), (6, "Leonard"), (3, "Steve")))
    }
  }

  @Test def testUnionWithoutProjection() = db withSession { implicit ss: Session =>

    Managers.ddl create;
    Managers.insertAll(
      (1, "Peter", "HR"),
      (2, "Amy", "IT"),
      (3, "Steve", "IT")
    )

    def f(s: String) = Managers filter { _.name =~ s }
    val q = f("Peter") union f("Amy")
    q.dump("q: ")
    println(q.selectStatement)
    assertEquals(Set((1, "Peter", "HR"), (2, "Amy", "IT")), q.list().toSet)
  }

  /**
   * in libraries like Slick (Slaq shares same root) you can only select a projection
   * of columns or an entire table in a UNION query. This test demonstrates new
   * functionality whereby you can select single columns and MappedProjections (of tables)
   * in a UNION query.
   *
   * IMPORTANT note:
   *   1) yield of tables MUST use projection t.*, and not just table alias t
   *   2) order of yield columns matters since top level select is:
   *      "SELECT t1.* FROM ((SELECT ...) UNION (SELECT ...)) t1", so yield columns must
   *      match exactly the order of select columns.
   */
  @Test def testUnionWithStarProjection() = db withSession { implicit ss: Session =>

    case class Managers(id: Int, name: String, locId: Option[Int] = None)
    case class Employees(id: Int, name: String, locId: Option[Int] = None)
    case class Locations(id: Int, name: String)

    object Managers extends Table[Managers]("managers") {
      def id = column[Int]("id")
      def name = column[String]("name")
      def locId = column[Option[Int]]("locId")
      def * = id ~ name ~ locId <> (Managers.apply _, x => Tuple.fromProductTyped(x))
    }

    object Employees extends Table[Employees]("employees") {
      def id = column[Int]("id")
      def name = column[String]("name")
      def locId = column[Option[Int]]("locId")
      def * = id ~ name ~ locId <> (Employees.apply _, x => Tuple.fromProductTyped(x))
    }

    object Locations extends Table[Locations]("locations") {
      def id = column[Int]("id")
      def name = column[String]("name")
      def * = id ~ name <> (Locations.apply _, x => Tuple.fromProductTyped(x))
    }

    (Managers.ddl ++ Employees.ddl ++ Locations.ddl) create

    Managers.insertAll(
      Managers(1, "Peter"),
      Managers(2, "Jennifer"),
      Managers(3, "Steve")
    )

    Employees.insertAll(
      Employees(1, "Peter"),
      Employees(2, "Jennifer"),
      Employees(3, "Leonard"),
      Employees(4, "Ben"),
      Employees(5, "Greg")
    )

    def fn(forCount: Boolean) = for {
      (m, e) <- Managers join Employees on (_.id is _.id) if (
        e.name =~ "Jennifer" | m.name =~ "Peter"
      )
      l <- Locations leftJoin (_.id is m.locId)
      _ <- Query groupBy(m.id) if forCount
    } yield (if forCount then m.id.count else ConstColumn(0), m.*, e.*, l.*)

    val q4 = for (cnt, m, e, l) <- fn(true) union fn(false) yield (cnt, m, e, l)
    println("Star projection: " + q4.selectStatement)
    q4.foreach(o => println("  " + o))
    assertEquals(
      q4.list(),
      List(
        (1,Managers(1,"Peter",None),Employees(1,"Peter",None),Locations(0,"")),
        (1,Managers(2,"Jennifer",None),Employees(2,"Jennifer",None),Locations(0,""))
      )
    )
  }
}
