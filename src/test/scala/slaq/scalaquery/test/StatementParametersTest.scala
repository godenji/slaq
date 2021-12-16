package slaq.test

import org.junit.Test
import org.junit.Assert._
import slaq.session._
import slaq.test.util._
import slaq.test.util.TestDB._

object StatementParametersTest extends DBTestObject(H2Mem)

class StatementParametersTest(tdb: TestDB) extends DBTest(tdb) {

  @Test def testExplicit(): Unit = {
    println("*** Explicit ***")
    db withSession { s1 =>
      pr("start")(using s1)
      ResultSetType.ScrollInsensitive(s1) { s2 =>
        pr("in ScrollInsensitive block")(using s2)
        ResultSetHoldability.HoldCursorsOverCommit(s2) { s3 =>
          pr("in HoldCursorsOverCommit block")(using s3)
          check(ResultSetType.Auto, ResultSetConcurrency.Auto, ResultSetHoldability.Auto)(using s1)
          check(ResultSetType.ScrollInsensitive, ResultSetConcurrency.Auto, ResultSetHoldability.Auto)(using s2)
          check(ResultSetType.ScrollInsensitive, ResultSetConcurrency.Auto, ResultSetHoldability.HoldCursorsOverCommit)(using s3)
        }
        pr("back out")(using s2)
      }
      pr("back out")(using s1)
    }
  }

  @Test def testImplicit(): Unit = {
    println("*** Implicit ***")
    db withSession { implicit session =>
      pr("start")
      check(ResultSetType.Auto, ResultSetConcurrency.Auto, ResultSetHoldability.Auto)
      /*
       * need explicit reference to ResultSet*'s session to override above withSession implicit
       * looks like in order to set custom ResultSet* attribs one *must* be explicit
       */
      ResultSetType.ScrollInsensitive { (ss: Session) =>
        pr("in ScrollInsensitive block")(using ss)
        check(ResultSetType.ScrollInsensitive, ResultSetConcurrency.Auto, ResultSetHoldability.Auto)
        ResultSetHoldability.HoldCursorsOverCommit { (ss: Session) =>
          pr("in HoldCursorsOverCommit block")(using ss)
          check(ResultSetType.ScrollInsensitive, ResultSetConcurrency.Auto, ResultSetHoldability.HoldCursorsOverCommit)
        }
        pr("back out")
        check(ResultSetType.ScrollInsensitive, ResultSetConcurrency.Auto, ResultSetHoldability.Auto)
      }
      pr("back out")
      check(ResultSetType.Auto, ResultSetConcurrency.Auto, ResultSetHoldability.Auto)
    }
  }

  def pr(msg: String)(using ss: Session) =
    println(
      s"$msg: ${ss.cursorType} ${ss.concurrencyType} ${ss.holdabilityType}"
    )

  def check(t: ResultSetType, c: ResultSetConcurrency, h: ResultSetHoldability)(using ss: Session): Unit = {
    assertEquals(ss.cursorType, t)
    assertEquals(ss.concurrencyType, c)
    assertEquals(ss.holdabilityType, h)
  }
}
