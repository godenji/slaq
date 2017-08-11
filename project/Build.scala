import sbt._
import Keys._
import scala.language.postfixOps

object ApplicationBuild
	extends meta.Build with Dependencies with MyBuildSettings {
	
  //override val scalaRelease = "2.12.1"

  val repoKind = SettingKey[String](
  	"repo-kind", "Maven repository kind (\"snapshots\" or \"releases\")"
  )

  /* FMPP Task */
  lazy val fmpp = TaskKey[Seq[File]]("fmpp")
  lazy val fmppConfig = config("fmpp") hide
  lazy val fmppSettings =
  	inConfig(Compile)(Seq(sourceGenerators += fmpp, fmpp := fmppTask.value)) ++ 
  	Seq(
	    libraryDependencies += (
	    	"net.sourceforge.fmpp" % "fmpp" % "0.9.15" % fmppConfig.name
	    ),
	    ivyConfigurations += fmppConfig,
	    fullClasspath in fmppConfig := (
	    	update.map(
	    		_ select configurationFilter(fmppConfig.name) map Attributed.blank
	    	)
    	).value, 
    	// Add generated sources to sources JAR
	    mappings in (Compile, packageSrc) ++= {
	    	val(base, srcs, srcDir) = (
	    		(sourceManaged in Compile).value,
	    		(managedSources in Compile).value,
	    		(sourceDirectory in Compile).value
	    	)
	      val fmppSrc = srcDir / "scala"
	      val inFiles = fmppSrc ** "*.fm"
	      (srcs pair (Path.relativeTo(base) | Path.flat)) ++
	        (inFiles pair (Path.relativeTo(fmppSrc) | Path.flat))
	    }
	  )
	  
  lazy val fmppTask = Def.task {
  	val(cp, r, output, s, srcDir) = (
  		(fullClasspath in fmppConfig).value, (runner in fmpp).value, 
  		sourceManaged.value, streams.value, sourceDirectory.value
  	)
    val fmppSrc = srcDir / "scala"
    val inFiles = (fmppSrc ** "*.fm" get).toSet
    val cachedFun =
			FileFunction.cached(s.cacheDirectory / "fmpp", outStyle = FilesInfo.exists) {
				(in: Set[File]) =>

				IO.delete(output ** "*.scala" get)
				val args = List(
					"--expert", "-q", "-S", fmppSrc.getPath, "-O", output.getPath,
					"--replace-extensions=fm, scala", "-M", "execute(**/*.fm), ignore(**/*)"
				)
        r.run("fmpp.tools.CommandLine", cp.files, args, s.log).foreach(
          sys.error
        )
				(output ** "*.scala").get.toSet
			}
    cachedFun(inFiles).toSeq
  }
}

