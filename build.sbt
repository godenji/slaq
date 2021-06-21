import ApplicationBuild._

lazy val root = (project in file(".")).
  settings(publishSettings("slaq")).
  settings(fmppSettings).
  settings(scalaFixSettings).
  settings(
    name := "slaq",
    description := "A type-safe database API for Scala",
    organization := "io.github.godenji",
    sonatypeProfileName in ThisBuild := organization.value,
    version := "0.10.9",
    scalaVersion := "2.13.6",
    scalacOptions ++= Seq(
      "-opt:l:inline",
      "-unchecked", "-deprecation", "-feature",
      "-Ywarn-unused:-implicits",
      "-language:implicitConversions", "-language:postfixOps",
      "-language:higherKinds", "-language:existentials"
    ),
    libraryDependencies ++= appDeps,
    credentials ++= {
      val creds = Path.userHome / ".sonatype" / organization.value
      if (creds.exists) Seq(Credentials(creds)) else Nil
    },
    testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v"),
    parallelExecution in Test := false,
    logBuffered := false
  ).
  enablePlugins(BuildInfoPlugin)

def scalaFixSettings = Seq(
  semanticdbEnabled := true,
  semanticdbOptions += "-P:semanticdb:synthetics:on",
  semanticdbVersion := scalafixSemanticdb.revision,
  ThisBuild / scalafixScalaBinaryVersion := CrossVersion.binaryScalaVersion(scalaVersion.value),
  scalafixDependencies += "org.scala-lang" %% "scala-rewrites" % "0.1.3"
)

def publishSettings(projectName: String) = Seq(
  pomExtra := pomDetail,
  publishMavenStyle := true,
  publishArtifact in Test := false,
  publishArtifact in (Compile, packageDoc) := true,
  publishArtifact in (Compile, packageSrc) := true,
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
