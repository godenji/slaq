import sbt._
import Keys._
import com.typesafe.sbteclipse.core.EclipsePlugin.EclipseKeys

object ScalaQueryBuild 
	extends Build with Transformers with Settings {

  val repoKind = SettingKey[String]("repo-kind", "Maven repository kind (\"snapshots\" or \"releases\")")

  lazy val superSettings = super.settings
  lazy val root = Project(id = "scala-query", base = file("."), settings = _settings).settings(
  	Project.defaultSettings ++ fmppSettings ++ Seq(
    	name := appName, version := appVersion,
			organizationName := "ScalaQuery", organization := "org.scalaquery",
			scalaVersion := scalaRelease,
			scalacOptions ++= Seq(
				"-language:implicitConversions", "-language:postfixOps", 
				"-language:higherKinds", "-language:existentials",
				"-feature", "-deprecation"/*, "-optimise"*/, "-Yinline-warnings"
			),
			description := "A type-safe database API for Scala",
			homepage := Some(url("http://scalaquery.org/")),
			startYear := Some(2008),
			licenses += ("Two-clause BSD-style license", url("http://github.com/szeiger/scala-query/blob/master/LICENSE.txt")),
			testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v"),
			libraryDependencies ++= Seq(
			  "com.h2database" % "h2" % "1.4.185" % "test",
			  "org.xerial" % "sqlite-jdbc" % "3.8.7" % "test",
			  "org.apache.derby" % "derby" % "10.11.1.1" % "test",
			  "org.hsqldb" % "hsqldb" % "2.3.2" % "test",
			  "postgresql" % "postgresql" % "9.1-901.jdbc4" % "test",
			  "mysql" % "mysql-connector-java" % "5.1.34" % "test",
			  "net.sourceforge.jtds" % "jtds" % "1.3.1" % "test",
			  "com.novocode" % "junit-interface" % "0.11" % "test"
			),
      repoKind <<= (version)(v => if(v.trim.endsWith("SNAPSHOT")) "snapshots" else "releases"),
      scalacOptions in doc <++= (version).map(v => Seq("-doc-title", "ScalaQuery", "-doc-version", v)),
      parallelExecution in Test := false,
      logBuffered := false,
      offline := true,
      makePomConfiguration ~= { _.copy(configurations = Some(Seq(Compile, Runtime))) }
  	):_*
  )

  /* FMPP Task */
  lazy val fmpp = TaskKey[Seq[File]]("fmpp")
  lazy val fmppConfig = config("fmpp") hide
  lazy val fmppSettings = inConfig(Compile)(Seq(sourceGenerators <+= fmpp, fmpp <<= fmppTask)) ++ Seq(
    libraryDependencies += "net.sourceforge.fmpp" % "fmpp" % "0.9.14" % fmppConfig.name,
    ivyConfigurations += fmppConfig,
    fullClasspath in fmppConfig <<= update map { _ select configurationFilter(fmppConfig.name) map Attributed.blank },
    //mappings in (Compile, packageSrc) <++= // Add generated sources to sources JAR
    //  (sourceManaged in Compile, managedSources in Compile) map { (b, s) => s x (Path.relativeTo(b) | Path.flat) }
    mappings in (Compile, packageSrc) <++=
      (sourceManaged in Compile, managedSources in Compile, sourceDirectory in Compile) map { (base, srcs, srcDir) =>
        val fmppSrc = srcDir / "scala"
        val inFiles = fmppSrc ** "*.fm"
        (srcs x (Path.relativeTo(base) | Path.flat)) ++ // Add generated sources to sources JAR
          (inFiles x (Path.relativeTo(fmppSrc) | Path.flat)) // Add *.fm files to sources JAR
      }
  )
  lazy val fmppTask =
    (fullClasspath in fmppConfig, runner in fmpp, sourceManaged, streams, cacheDirectory, sourceDirectory) map { (cp, r, output, s, cache, srcDir) =>
      val fmppSrc = srcDir / "scala"
      val inFiles = (fmppSrc ** "*.fm" get).toSet
      val cachedFun = FileFunction.cached(cache / "fmpp", outStyle = FilesInfo.exists) { (in: Set[File]) =>
        IO.delete(output ** "*.scala" get)
        val args = "--expert" :: "-q" :: "-S" :: fmppSrc.getPath :: "-O" :: output.getPath ::
          "--replace-extensions=fm, scala" :: "-M" :: "execute(**/*.fm), ignore(**/*)" :: Nil
        toError(r.run("fmpp.tools.CommandLine", cp.files, args, s.log))
        (output ** "*.scala").get.toSet
      }
      cachedFun(inFiles).toSeq
    }
}

