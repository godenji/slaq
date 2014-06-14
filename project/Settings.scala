import sbt._
import Keys._
import com.typesafe.sbteclipse.core.EclipsePlugin.{
  EclipseKeys,EclipseExecutionEnvironment
}

trait Settings {
  self: Transformers =>
  
  def superSettings: Seq[Setting[_]]
  protected def _settings: Seq[Setting[_]] = { 
    superSettings ++ eclipseSettings ++ ivySettings ++ Seq(
    	//incOptions := incOptions.value.withNameHashing(true),
    	doc in Compile <<= target.map(_ / "none") // don't generate API docs in dist
    )
  }
  
  private def eclipseSettings = Seq(
		EclipseKeys.projectTransformerFactories := Seq(addLinkedResource),
		EclipseKeys.classpathTransformerFactories := Seq(addSourcesManaged),
		EclipseKeys.preTasks := Seq(), // don't trigger compile on project gen
		EclipseKeys.eclipseOutput := Some(
			// direct eclipse output folder to sbt target
			// sbt runs the show so we never have to clean/build in Eclipse
			s"${tmpfs}/${appName}/scala-${scalaRelease.take(4)}/classes"
		)
	)
  
  private def ivySettings = Seq(
  	// prevent deps update check on every clean/compile by moving
    // sbt cache config & update dirs away from clean's reach
  	cacheDirectory <<= baseDirectory / "sbt-cache-update",
  	cleanKeepFiles <+= target / "streams",
  	ivyConfiguration <<= (ivyConfiguration) map {
	  	case (c: InlineIvyConfiguration) => import c._
	  		new InlineIvyConfiguration(
	  			paths, resolvers, otherResolvers, moduleConfigurations, localOnly, lock, checksums, 
	  			resolutionCacheDir.map(_=> baseDirectory / "sbt-cache-config"), log
	  		)
	  	case x=>x
  	}
  )
    
}