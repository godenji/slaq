import sbt._
import Keys._

trait Dependencies{
	val appDeps = Seq(
	  "com.h2database" % "h2" % "1.4.185" % "test",
	  "org.xerial" % "sqlite-jdbc" % "3.8.7" % "test",
	  "org.hsqldb" % "hsqldb" % "2.3.2" % "test",
	  "org.postgresql" % "postgresql" % "9.4-1201-jdbc41" % "test",
	  "mysql" % "mysql-connector-java" % "5.1.34" % "test",
	  "net.sourceforge.jtds" % "jtds" % "1.3.1" % "test",
	  "com.novocode" % "junit-interface" % "0.11" % "test"
	)
}
