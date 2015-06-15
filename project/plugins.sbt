logLevel := Level.Warn

resolvers ++= Seq(
	Resolver.file(
		"godenji local", file("~/.ivy2/local/godenji")
	)(Resolver.ivyStylePatterns)
)

addSbtPlugin("godenji" % "eclipse-common" % "1.0.1-SNAPSHOT")
