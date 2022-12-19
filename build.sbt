import ApplicationBuild._

val scala3Version = "3.2.2-RC1"

lazy val root = (project in file(".")).
  settings(publishSettings("slaq")).
  settings(fmppSettings).
  settings(
    name := "slaq",
    description := "A type-safe database API for Scala",
    organization := "io.github.godenji",
    ThisBuild / sonatypeProfileName := organization.value,
    version := "1.0.0-M1",
    scalaVersion := scala3Version,
    scalacOptions ++= Seq(
      "-unchecked",
      "-deprecation",
      "-feature",
      "-language:implicitConversions",
      "-language:postfixOps",
      "-language:higherKinds",
      "-language:existentials",
      //
      //"-rewrite", "-source", "3.0-migration",
      //"-Wconf:unused:error",
      "-Xmigration",
      "-Yno-generic-signatures",
      "-Yno-kind-polymorphism"
    ),
    libraryDependencies ++= appDeps,
    credentials ++= {
      val creds = Path.userHome / ".sonatype" / organization.value
      if (creds.exists) Seq(Credentials(creds)) else Nil
    },
    testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v"),
    Test / parallelExecution := false,
    logBuffered := false
  )
  .enablePlugins(BuildInfoPlugin)

def publishSettings(projectName: String) = Seq(
  pomExtra := pomDetail,
  publishMavenStyle := true,
  Test / publishArtifact := false,
  Compile / packageDoc / publishArtifact := true,
  Compile / packageSrc / publishArtifact := true,
  pomIncludeRepository := { _ => false },
  buildInfoKeys := Seq[BuildInfoKey](version),
  buildInfoPackage := projectName,
  publishTo := getPublishToRepo.value
)

def getPublishToRepo = Def.setting {
  if (isSnapshot.value)
    Some(Opts.resolver.sonatypeSnapshots)
  else
    Some(Opts.resolver.sonatypeStaging)
}

def pomDetail =
  <inceptionYear>2014</inceptionYear>
  <url>https://github.com/godenji/slaq</url>
  <licenses>
    <license>
      <name>Two-clause BSD-style license</name>
      <url>https://github.com/godenji/slaq/blob/master/LICENSE.txt</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>git@github.com:godenji/slaq.git</url>
    <connection>scm:git:git@github.com:godenji/slaq</connection>
  </scm>
  <developers>
    <developer>
      <id>szeiger</id>
      <name>Stefan Zeiger</name>
      <url>http://szeiger.de</url>
    </developer>
    <developer>
      <id>godenji</id>
      <name>N.S. Cutler</name>
      <url>https://github.com/godenji</url>
    </developer>
  </developers>
