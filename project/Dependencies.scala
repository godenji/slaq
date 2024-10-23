import sbt._
import Keys._

trait Dependencies {
  val appDeps = Seq(
    "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",
    "com.h2database" % "h2" % "1.4.200" % "test",
    "org.xerial" % "sqlite-jdbc" % "3.36.0.3" % "test",
    "org.hsqldb" % "hsqldb" % "2.7.3" % "test",
    "org.postgresql" % "postgresql" % "42.7.4" % "test",
    "com.mysql" % "mysql-connector-j" % "9.1.0" % "test",
    "net.sourceforge.jtds" % "jtds" % "1.3.1" % "test",
    "com.github.sbt" % "junit-interface" % "0.13.3" % "test"
  )
}
