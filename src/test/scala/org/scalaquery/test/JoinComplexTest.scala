//package org.scalaquery.test
//
//import org.junit.After
//import org.junit.Test
//import org.junit.Assert._
//import org.scalaquery.ql._
//import org.scalaquery.ql.TypeMapper._
//import org.scalaquery.ql.driver.SQLiteDriver
//import org.scalaquery.meta.MTable
//import org.scalaquery.session._
//import org.scalaquery.test.util._
//import org.scalaquery.test.util.TestDB._
//
//object JoinComplexTest extends DBTestObject(H2Mem
//	/*, Postgres, MySQL, DerbyMem, HsqldbMem, SQLiteMem, SQLServer*/
//)
//
//class JoinComplexTest(tdb: TestDB) extends DBTest(tdb) {
//  import tdb.driver.Implicit._
//  
//  case class A(id: Int, name: String)
//  case class B(id: Int, name: String)
//  case class C(id: Int, name: String)
//  case class D(id: Int, name: String)
//  case class E(id: Int, name: String)
//  case class F(id: Int, name: String)
//  
//  object As extends Table[A]("tableA") {
//  	def id = column[Int]("id")
//    def name = column[String]("name")
//    def * = id ~ name <> (A, A.unapply _)
//  }
//  object Bs extends Table[B]("tableB") {
//  	def id = column[Int]("id")
//    def name = column[String]("name")
//    def * = id ~ name <> (B, B.unapply _)
//  }
//  object Cs extends Table[C]("tableC") {
//  	def id = column[Int]("id")
//    def name = column[String]("name")
//    def aKey = foreignKey("foo", id, As)(_.id)
//    def * = id ~ name <> (C, C.unapply _)
//  }
//  object Ds extends Table[D]("tableD") {
//  	def id = column[Int]("id")
//    def name = column[String]("name")
//    def * = id ~ name <> (D, D.unapply _)
//  }
//  object Es extends Table[E]("tableE") {
//  	def id = column[Int]("id")
//    def name = column[String]("name")
//    def * = id ~ name <> (E, E.unapply _)
//  }
//  object Fs extends Table[F]("tableF") {
//  	def id = column[Int]("id")
//    def name = column[String]("name")
//    def * = id ~ name <> (F, F.unapply _)
//  }
//
//  @Test def test(): Unit = db withSession { implicit ss:Session=>
//
//    (As.ddl ++ Bs.ddl ++ Cs.ddl ++ Ds.ddl ++ Es.ddl ++ Fs.ddl) create
//
////    val q4 = for{
////    	a <- As
////    	b <- Bs if a.id is b.id
////    	c <- Cs if c.id is b.id
////    	d <- Ds if d.id is c.id
////    } yield(a,c,b,d)
////    println(q4.printable)
//    
////    val q4 = for{
////    	<&(a,b) <- Cs join As using(_.aKey)
////    } yield(b,a)
////    println(q4.printable)
//    
//    val q3 = for{
//    	<|(a,b) <- As join Bs on(_.id is _.id)
//    	<|(c,_) <- Cs leftJoin a left(_.id is a.id)
//    	<|(d,_) <- Ds leftJoin c left(_.id is c.id)
//    	<|(e,_) <- Es leftJoin d left(_.id is d.id)
//    	_ <- Query groupBy(c.id)
//    } yield (a.*,d.id,b.*,c.*)
//    println(q3.printable)
//    
////    val q1 = for{
////    	<|(a,b) <- As join Bs on(_.id is _.id)
////    	<|(c,_) <- Cs join a left(_.id is a.id)
////    	<|(d,_) <- Ds join c left(_.id is c.id)
////    } yield (a,b,c,d)
////    val q2 = for{
////    	(a,b,c,d) <- q1
////    	<|(_,e) <- c leftJoin Es right(_.id is c.id)
////    	<|(_,f) <- d leftJoin Fs right(_.id is d.id)
////    } yield (a.*,b.id,c.*,d.*,e.id,f)
////    println(q2.printable)
//    assert(false)
//  }
//}
