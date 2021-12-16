package slaq.test.util

import java.util.Properties
import slaq.ql.core.Profile
import slaq.ql.driver.{
  H2Driver,
  SQLiteDriver,
  PostgresDriver,
  MySQLDriver,
  HsqldbDriver
}
import slaq.ResultSetInvoker
import slaq.session._
import slaq.simple.{StaticQuery => Q}
import slaq.simple.GetResult.{given, *}
import java.util.zip.GZIPInputStream
import java.io._
import org.junit.Assert

object TestDBOptions {
  val testDBDir = dbProps.getProperty("testDir", "test-dbs")
  def testDBPath = {
    val f = new File(testDBDir)
    val s = f.getPath().replace('\\', '/')
    if (f.isAbsolute) s else "./" + s
  }
  lazy val dbProps = {
    val p = new Properties
    val f = new File("test-dbs", "databases.properties")
    if (f.isFile) {
      val in = new FileInputStream(f)
      try { p.load(in) } finally { in.close() }
    }
    p
  }
  lazy val testDBs = Option(dbProps.getProperty("testDBs")).map(_.split(',').map(_.trim).toSet)
  def isInternalEnabled(db: String) = testDBs.map(_.contains(db)).getOrElse(true)
  def isExternalEnabled(db: String) = isInternalEnabled(db) && "true" == dbProps.getProperty(db + ".enabled")
  def get(db: String, o: String) = Option(dbProps.getProperty(db + "." + o))
}

abstract class TestDB(val confName: String) {
  override def toString = url
  val url: String
  val jdbcDriver: String
  val driver: Profile
  def dbName = ""
  def userName = ""
  def createDB() = Database.forURL(url, driver = jdbcDriver)
  def cleanUpBefore() = cleanUp()
  def cleanUpAfter() = cleanUp()
  def cleanUp(): Unit = {}
  def deleteDBFiles(prefix: String): Unit = {
    def deleteRec(f: File): Boolean = {
      if (f.isDirectory()) f.listFiles.forall(deleteRec _) && f.delete()
      else f.delete()
    }
    val dir = new File(TestDBOptions.testDBDir)
    if (!dir.isDirectory) throw new IOException("Directory " + TestDBOptions.testDBDir + " not found")
    for (f <- dir.listFiles if f.getName startsWith prefix) {
      val p = TestDBOptions.testDBDir + "/" + f.getName
      if (deleteRec(f)) println("[Deleted database file " + p + "]")
      else throw new IOException("Couldn't delete database file " + p)
    }
  }
  def isEnabled = TestDBOptions.isInternalEnabled(confName)
  def getLocalTables(using Session): List[String] = {
    val tables = ResultSetInvoker[(String, String, String)](_.conn.getMetaData().getTables("", "", null, null)) {
      r => (r<<, r<<, r<<)
    }
    tables.list().map(_._3).sorted
  }
  def assertTablesExist(tables: String*)(using Session): Unit = {
    for (t <- tables) {
      try Q[Int] + "select 1 from " + driver.sqlUtils.quote(t) + " where 1 < 0" list() catch {
        case _: Exception =>
          Assert.fail("Table " + t + " should exist")
      }
    }
  }
  def assertNotTablesExist(tables: String*)(using Session): Unit = {
    for (t <- tables) {
      try {
        Q[Int] + "select 1 from " + driver.sqlUtils.quote(t) + " where 1 < 0" list();
        Assert.fail("Table " + t + " should not exist")
      }
      catch { case _: Exception => }
    }
  }
  def assertUnquotedTablesExist(tables: String*)(using Session): Unit = {
    for (t <- tables) {
      try Q[Int] + "select 1 from " + t + " where 1 < 0" list() catch {
        case _: Exception =>
          Assert.fail("Table " + t + " should exist")
      }
    }
  }
  def assertNotUnquotedTablesExist(tables: String*)(using Session): Unit = {
    for (t <- tables) {
      try {
        Q[Int] + "select 1 from " + t + " where 1 < 0" list();
        Assert.fail("Table " + t + " should not exist")
      }
      catch { case _: Exception => }
    }
  }
  def copy(src: File, dest: File): Unit = {
    dest.createNewFile()
    val out = new FileOutputStream(dest)
    try {
      var in: InputStream = new FileInputStream(src)
      try {
        if (src.getName.endsWith(".gz")) in = new GZIPInputStream(in)
        val buf = new Array[Byte](4096)
        var cont = true
        while (cont) {
          val len = in.read(buf)
          if (len < 0) cont = false
          else out.write(buf, 0, len)
        }
      }
      finally in.close()
    }
    finally out.close()
  }
  def canGetLocalTables = true
}

class SQLiteTestDB(dburl: String, confName: String) extends TestDB(confName) {
  val url = dburl
  val jdbcDriver = "org.sqlite.JDBC"
  val driver = SQLiteDriver
  override def getLocalTables(using Session) =
    super.getLocalTables.filter(s => !s.toLowerCase.contains("sqlite_"))
}

class ExternalTestDB(confName: String, val driver: Profile) extends TestDB(confName) {
  val jdbcDriver = TestDBOptions.get(confName, "driver").orNull
  val urlTemplate = TestDBOptions.get(confName, "url").getOrElse("")
  override def dbName = TestDBOptions.get(confName, "testDB").getOrElse("")
  val url = urlTemplate.replace("[DB]", dbName)
  val configuredUserName = TestDBOptions.get(confName, "user").orNull
  val password = TestDBOptions.get(confName, "password").orNull
  override def userName = TestDBOptions.get(confName, "user").orNull

  val adminDBURL = urlTemplate.replace("[DB]", TestDBOptions.get(confName, "adminDB").getOrElse(""))
  val create = TestDBOptions.get(confName, "create").getOrElse("").replace("[DB]", dbName).replace("[DBPATH]", new File(TestDBOptions.testDBDir).getAbsolutePath)
  val drop = TestDBOptions.get(confName, "drop").getOrElse("").replace("[DB]", dbName).replace("[DBPATH]", new File(TestDBOptions.testDBDir).getAbsolutePath)

  override def isEnabled = TestDBOptions.isExternalEnabled(confName)

  override def createDB() = Database.forURL(url, driver = jdbcDriver, user = configuredUserName, password = password)

  override def cleanUpBefore(): Unit = {
    if (drop.length > 0 || create.length > 0) {
      println("[Creating test database " + this + "]")
      Database.forURL(adminDBURL, driver = jdbcDriver, user = configuredUserName, password = password) withSession { implicit s: Session =>
        Q.u + drop execute();
        Q.u + create execute()
      }
    }
  }

  override def cleanUpAfter(): Unit = {
    if (drop.length > 0) {
      println("[Dropping test database " + this + "]")
      Database.forURL(adminDBURL, driver = jdbcDriver, user = configuredUserName, password = password) withSession { implicit s: Session =>
        Q.u + drop execute()
      }
    }
  }
}

abstract class HsqlDB(confName: String) extends TestDB(confName) {
  val jdbcDriver = "org.hsqldb.jdbcDriver"
  val driver = HsqldbDriver
  override def getLocalTables(using Session): List[String] = {
    val tables = ResultSetInvoker[(String, String, String)](_.conn.getMetaData().getTables(null, "PUBLIC", null, null)) {
      r => (r<<, r<<, r<<)
    }
    tables.list().map(_._3).sorted
  }
  override def userName = "sa"
}

object TestDB {
  type TestDBSpec = (DBTestObject => TestDB)

  def H2Mem(to: DBTestObject) = new TestDB("h2mem") {
    val url = "jdbc:h2:mem:test1"
    val jdbcDriver = "org.h2.Driver"
    val driver = H2Driver
    override val dbName = "test1"
  }

  def H2Disk(to: DBTestObject) = new TestDB("h2disk") {
    override val dbName = "h2-" + to.testClassName
    val url = "jdbc:h2:" + TestDBOptions.testDBPath + "/" + dbName
    val jdbcDriver = "org.h2.Driver"
    val driver = H2Driver
    override def cleanUp() = deleteDBFiles(dbName)
  }

  def HsqldbMem(to: DBTestObject) = new HsqlDB("hsqldbmem") {
    override val dbName = "test1"
    val url = "jdbc:hsqldb:mem:" + dbName + ";user=SA;password=;shutdown=true"
  }

  def HsqldbDisk(to: DBTestObject) = new HsqlDB("hsqldbmem") {
    override val dbName = "hsqldb-" + to.testClassName
    val url = "jdbc:hsqldb:file:" + TestDBOptions.testDBPath + "/" + dbName + ";user=SA;password=;shutdown=true;hsqldb.applog=0"
    override def cleanUp() = deleteDBFiles(dbName)
  }

  def SQLiteMem(to: DBTestObject) = new SQLiteTestDB("jdbc:sqlite::memory:", "sqlitemem") {
    override val dbName = ":memory:"
  }

  def SQLiteDisk(to: DBTestObject) = {
    val prefix = "sqlite-" + to.testClassName
    new SQLiteTestDB("jdbc:sqlite:" + TestDBOptions.testDBPath + "/" + prefix + ".db", "sqlitedisk") {
      override val dbName = prefix
      override def cleanUp() = deleteDBFiles(prefix)
    }
  }

  def Postgres(to: DBTestObject) = new ExternalTestDB("postgres", PostgresDriver) {
    override def getLocalTables(using session: Session) = {
      val tables = ResultSetInvoker[(String, String, String)](_.conn.getMetaData().getTables("", "public", null, null)) {
        r => (r<<, r<<, r<<)
      }
      tables.list().map(_._3).filter(s => !s.toLowerCase.endsWith("_pkey") && !s.toLowerCase.endsWith("_id_seq")).sorted
    }
  }

  def MySQL(to: DBTestObject) = new ExternalTestDB("mysql", MySQLDriver) {
    override def userName = super.userName + "@localhost"
  }
}
