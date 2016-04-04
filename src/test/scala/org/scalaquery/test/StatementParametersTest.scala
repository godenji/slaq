package org.scalaquery.test

import org.junit.Test
import org.junit.Assert._
import org.scalaquery.session._
import org.scalaquery.test.util._
import org.scalaquery.test.util.TestDB._

object StatementParametersTest extends DBTestObject(H2Mem)

class StatementParametersTest(tdb: TestDB) extends DBTest(tdb) {
  import tdb.driver.Implicit._

  @Test def testExplicit() {
    println("*** Explicit ***")
    db withSession { s1:Session =>
      pr("start")(s1)
      ResultSetType.ScrollInsensitive(s1) { s2 =>
        pr("in ScrollInsensitive block")(s2)
        ResultSetHoldability.HoldCursorsOverCommit(s2) { s3 =>
          pr("in HoldCursorsOverCommit block")(s3)
          check(ResultSetType.Auto, ResultSetConcurrency.Auto, ResultSetHoldability.Auto)(s1)
          check(ResultSetType.ScrollInsensitive, ResultSetConcurrency.Auto, ResultSetHoldability.Auto)(s2)
          check(ResultSetType.ScrollInsensitive, ResultSetConcurrency.Auto, ResultSetHoldability.HoldCursorsOverCommit)(s3)
        }
        pr("back out")(s2)
      }
      pr("back out")(s1)
    }
  }

  @Test def testImplicit() {
    println("*** Implicit ***")
    db withSession { implicit session:Session=>
      pr("start")
      check(ResultSetType.Auto, ResultSetConcurrency.Auto, ResultSetHoldability.Auto)
      /*
       * need explicit reference to ResultSet*'s session to override above withSession implicit
       * looks like in order to set custom ResultSet* attribs one *must* be explicit
       */
      ResultSetType.ScrollInsensitive {ss:Session=>
        pr("in ScrollInsensitive block")
        check(ResultSetType.ScrollInsensitive, ResultSetConcurrency.Auto, ResultSetHoldability.Auto)
        ResultSetHoldability.HoldCursorsOverCommit {ss:Session=>
          pr("in HoldCursorsOverCommit block")
          check(ResultSetType.ScrollInsensitive, ResultSetConcurrency.Auto, ResultSetHoldability.HoldCursorsOverCommit)
        }
        pr("back out")
        check(ResultSetType.ScrollInsensitive, ResultSetConcurrency.Auto, ResultSetHoldability.Auto)
      }
      pr("back out")
      check(ResultSetType.Auto, ResultSetConcurrency.Auto, ResultSetHoldability.Auto)
    }
  }

  def pr(msg: String)(implicit ss: Session) = println(
  	s"$msg: ${ss.cursorType} ${ss.concurrencyType} ${ss.holdabilityType}"
  )

  def check(t: ResultSetType, c: ResultSetConcurrency, h: ResultSetHoldability)(implicit ss: Session) {
    assertEquals(ss.cursorType, t)
    assertEquals(ss.concurrencyType, c)
    assertEquals(ss.holdabilityType, h)
  }
}
