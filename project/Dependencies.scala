import sbt._
import Keys._

trait Dependencies {
  val appDeps = Seq(
    "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",
    //"io.github.godenji" %% "isomorphic" % "0.1.8",
    "com.h2database" % "h2" % "1.4.200" % "test",
    "org.xerial" % "sqlite-jdbc" % "3.36.0.3" % "test",
    "org.hsqldb" % "hsqldb" % "2.6.1" % "test",
    "org.postgresql" % "postgresql" % "42.3.1" % "test",
    "mysql" % "mysql-connector-java" % "8.0.27" % "test",
    "net.sourceforge.jtds" % "jtds" % "1.3.1" % "test",
    "com.novocode" % "junit-interface" % "0.11" % "test"
  )
}
