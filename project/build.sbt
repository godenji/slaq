scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")
resolvers ++= Seq(
  "Scala 2.12.0-M4 Core" at "https://oss.sonatype.org/content/repositories/orgscala-lang-1304/",
  "Scala 2.12.0-M4 Modules" at "https://oss.sonatype.org/content/repositories/orgscala-lang-1305/"
)
