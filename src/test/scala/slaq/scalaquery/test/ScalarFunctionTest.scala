package slaq.test

import org.junit.Test
import org.junit.Assert._
import slaq.ql._
import slaq.ql.TypeMapper._
import slaq.session._
import slaq.test.util._
import slaq.test.util.TestDB._
import java.sql.{Time, Date, Timestamp}

object ScalarFunctionTest extends DBTestObject(H2Mem, SQLiteMem, Postgres, MySQL, HsqldbMem)

class ScalarFunctionTest(tdb: TestDB) extends DBTest(tdb) {
  import tdb.driver.Implicit.{given, *}

  @Test def test = db withSession { implicit ss: Session =>
    def check[T](q: Query[ColumnBase[T], T], exp: T*) = {
      println("Executing: " + q.selectStatement)
      assertEquals(exp.toSet, q.list().toSet)
    }
    def checkIn[T](q: Query[ColumnBase[T], T], exp: T*) = {
      println("Executing: " + q.selectStatement)
      val found = q.list().toSet
      assert(found.forall(exp contains _), "all of result " + found + " should be in expected " + exp)
    }

    // Literals
    def checkLit[T: TypeMapper](v: T) = check(Query(ConstColumn(v)), v)
    checkLit(false)
    checkLit(true)
    checkLit(42: Byte)
    checkLit(-42: Byte)
    checkLit(Date.valueOf("2011-07-15"))
    checkLit(Time.valueOf("15:53:21"))
    checkLit(Timestamp.valueOf("2011-07-15 15:53:21"))
    checkLit(42)
    checkLit(-42)
    checkLit(17.5)
    checkLit(-17.5)
    checkLit(17.5f)
    checkLit(-17.5f)
    checkLit(42L)
    checkLit(-42L)
    checkLit("foo")

    check(Query("42".asColumnOf[Int]), 42)
    check(Query(ConstColumn("foo").length), 3)
    check(Query(ConstColumn("foo") ++ "bar"), "foobar")
    check(Query(ConstColumn(1) ifNull 42), 1)
    check(Query(ConstColumn[Option[Int]](None) ifNull 42), 42)
    check(Query(ConstColumn(8) % 3), 2)
    check(Query(ConstColumn(-12.5).abs), 12.5)
    check(Query(ConstColumn(1.9).ceil), 2.0)
    check(Query(ConstColumn(1.5).ceil), 2.0)
    check(Query(ConstColumn(1.4).ceil), 2.0)
    check(Query(ConstColumn(-1.9).ceil), -1.0)
    check(Query(ConstColumn(-1.5).ceil), -1.0)
    check(Query(ConstColumn(-1.4).ceil), -1.0)
    check(Query(ConstColumn(1.5).floor), 1.0)
    check(Query(ConstColumn(1.4).floor), 1.0)
    check(Query(ConstColumn(-1.5).floor), -2.0)

    val myExpr = SimpleExpression.binary[Int, Int, Int] { (l, r, b, qb) =>
      b += '('
      qb.expr(l, b)
      b += '+'
      qb.expr(r, b)
      b += "+1)"
    }

    check(Query(myExpr(4, 5)), 10)
  }
}
