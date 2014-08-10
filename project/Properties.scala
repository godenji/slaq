trait Properties {
	val appName         = "scala-query"
  val appVersion      = "0.10.1"
	val scalaRelease		= "2.11.2"
	val tmpDir 					= "/tmp/sbt" // sbt compile target
	val tmpfs = 
		"TARGET_TMPFS" // linked resource, expands to abs path /tmp/sbt in eclipse
  
}
