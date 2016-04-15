import sbt._
import Keys._
import com.typesafe.sbteclipse.core.EclipsePlugin.EclipseKeys
import scala.language.postfixOps

object ApplicationBuild extends Build
	with meta.Build with Dependencies with MyBuildSettings {

  val repoKind = SettingKey[String]("repo-kind", "Maven repository kind (\"snapshots\" or \"releases\")")
  //override val scalaRelease = "2.12.0-M4"

  lazy val superSettings = super.settings
  lazy val root = Project(
  	appName, file("."), settings = _settings(
  		appName, Seq(Libs.isoMacro) ++ appDeps
  	)
  ).settings(
  	Defaults.coreDefaultSettings ++ fmppSettings ++ Seq(
    	name := appName,
			organizationName := "ScalaQuery", organization := "org.scalaquery",
			scalaVersion := scalaRelease,
			scalacOptions ++= Seq(
				//"-optimise", /* note: tests FAIL with optimise enabled */
				"-unchecked", "-deprecation", "-feature",
				"-Yinline-warnings", "-Ywarn-unused-import",
				"-language:implicitConversions", "-language:postfixOps", 
				"-language:higherKinds", "-language:existentials"
				//"-Yno-adapted-args:false", "-Xfatal-warnings:false"
			),
			description := "A type-safe database API for Scala",
			homepage := Some(url("http://scalaquery.org/")),
			startYear := Some(2008),
			licenses += ("Two-clause BSD-style license", url("http://github.com/szeiger/scala-query/blob/master/LICENSE.txt")),
			testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v"),
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
        (srcs pair (Path.relativeTo(base) | Path.flat)) ++ // Add generated sources to sources JAR
          (inFiles pair (Path.relativeTo(fmppSrc) | Path.flat)) // Add *.fm files to sources JAR
      }
  )
  lazy val fmppTask =
    (fullClasspath in fmppConfig, runner in fmpp, sourceManaged, streams, sourceDirectory) map { (cp, r, output, s, srcDir) =>
      val fmppSrc = srcDir / "scala"
      val inFiles = (fmppSrc ** "*.fm" get).toSet
      val cachedFun = FileFunction.cached(s.cacheDirectory / "fmpp", outStyle = FilesInfo.exists) { (in: Set[File]) =>
        IO.delete(output ** "*.scala" get)
        val args = "--expert" :: "-q" :: "-S" :: fmppSrc.getPath :: "-O" :: output.getPath ::
          "--replace-extensions=fm, scala" :: "-M" :: "execute(**/*.fm), ignore(**/*)" :: Nil
        toError(r.run("fmpp.tools.CommandLine", cp.files, args, s.log))
        (output ** "*.scala").get.toSet
      }
      cachedFun(inFiles).toSeq
    }
}

