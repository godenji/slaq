package slaq.test

import org.junit.Test
import org.junit.Assert._
import slaq.test.util._
import slaq.test.util.TestDB._
import slaq.session.Session

object EmbeddingTest extends DBTestObject(H2Mem)

class EmbeddingTest(tdb: TestDB) extends DBTest(tdb) {

  @Test def testRaw(): Unit = db withSession { implicit ss: Session =>
    import slaq.simple.{StaticQuery => Q, GetResult}

    Q.u + "create table USERS(ID int not null primary key, NAME varchar(255))" execute();
    Q.u + "create table POSTS(ID int not null primary key, NAME varchar(255), UID int not null)" execute();
    List(
      (1, "u1"),
      (2, "u2"),
      (3, "u3")
    ).foreach(Q.u1[(Int, String)] + "insert into USERS values (?, ?)" execute)
    List(
      (1, "p1u1", 1),
      (2, "p2u1", 1),
      (3, "p3u1", 1),
      (4, "p4u2", 2)
    ).foreach(Q.u1[(Int, String, Int)] + "insert into POSTS values (?, ?, ?)" execute)

    val l1 = Q(using GetResult { r => (r.nextString(), r.nextString()) }) + """
      select u.NAME, p.NAME
      from USERS u left join POSTS p on u.ID = p.UID
      order by u.NAME, p.NAME
    """ list();
    l1 foreach println
    assertEquals(List(
      ("u1", "p1u1"),
      ("u1", "p2u1"),
      ("u1", "p3u1"),
      ("u2", "p4u2"),
      ("u3", null)
    ), l1)
  }
}
