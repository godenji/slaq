package org.scalaquery.test

import org.junit.Test
import org.junit.Assert._
import org.scalaquery.ql._
import org.scalaquery.ql.TypeMapper._
import org.scalaquery.session._
import org.scalaquery.test.util._
import org.scalaquery.test.util.TestDB._
import godenji.iso._

object MacroTest extends DBTestObject(H2Mem)

case class UserId(value: Int) extends AnyVal with MappedTo[Int]

class MacroTest(tdb: TestDB) extends DBTest(tdb) {
  import tdb.driver.Implicit._

  @Test def testMappedEntity(): Unit = {

    case class User(id: UserId, first: String, last: String)

    object Users extends Table[User]("users") {
      def id = column[UserId]("id", O PrimaryKey, O AutoInc)
      def first = column[String]("first")
      def last = column[String]("last")
      def * = id ~ first ~ last <> (User.apply _, User.unapply _)
      val findByID = this.createFinderBy(_.id)
    }

    db withSession { implicit ss: Session =>

      Users.ddl.create

    }
  }

  @Test def testMappedType(): Unit = {

    sealed trait Bool
    case object True extends Bool
    case object False extends Bool

    implicit val boolTypeMapper = MappedTypeMapper.base[Bool, Int](
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
