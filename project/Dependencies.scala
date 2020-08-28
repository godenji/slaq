import sbt._
import Keys._

trait Dependencies {
  val appDeps = Seq(
    "org.scala-lang.modules" %% "scala-parallel-collections" % "0.2.0",
    "io.github.godenji" %% "isomorphic" % "0.1.7",
    "com.h2database" % "h2" % "1.4.191" % "test",
    "org.xerial" % "sqlite-jdbc" % "3.8.11.2" % "test",
    "org.hsqldb" % "hsqldb" % "2.3.3" % "test",
    "org.postgresql" % "postgresql" % "9.4.1208.jre7" % "test",
    "mysql" % "mysql-connector-java" % "5.1.38" % "test",
    "net.sourceforge.jtds" % "jtds" % "1.3.1" % "test",
    "com.novocode" % "junit-interface" % "0.11" % "test"
  )
}
