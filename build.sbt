import ApplicationBuild._

lazy val root = (project in file(".")).
  settings(
    _settings(
      appName, Seq(Libs.valueClassBindableRoot) ++ appDeps
    )
  ).
  settings(
    fmppSettings ++ Seq(
      name := appName,
      organizationName := "ScalaQuery", 
      organization := "org.scalaquery",
      scalaVersion := scalaRelease,
      scalacOptions ++= Seq(
        // note: tests FAIL with optimise enabled
        "-opt:l:inline",
        "-unchecked", "-deprecation", "-feature",
        "-Ywarn-unused-import",
        "-language:implicitConversions", "-language:postfixOps", 
        "-language:higherKinds", "-language:existentials"
      ),
      description := "A type-safe database API for Scala",
      homepage := Some(url("http://scalaquery.org/")),
      startYear := Some(2008),
      licenses += (
        "Two-clause BSD-style license", 
        url("http://github.com/szeiger/scala-query/blob/master/LICENSE.txt")
      ),
      testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v"),
      repoKind := ((Keys.version)(v => 
        if(v.trim.endsWith("SNAPSHOT")) "snapshots" else "releases")
      ).value,
      scalacOptions in doc ++= ((Keys.version).map(v => 
        Seq("-doc-title", "ScalaQuery", "-doc-version", v))
      ).value,
      parallelExecution in Test := false,
      logBuffered := false,
      offline := true,
      makePomConfiguration ~= (
        _.withConfigurations(Some(Vector(Compile, Runtime)))
      )
    ):_*
  )
