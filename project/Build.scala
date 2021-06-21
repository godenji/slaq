import sbt._
import Keys._
import scala.language.postfixOps

object ApplicationBuild
  extends Dependencies {

  lazy val fmpp = TaskKey[Seq[File]]("fmpp")
  lazy val FmppConfig = config("fmpp") hide
  lazy val fmppSettings =
    inConfig(Compile)(Seq(sourceGenerators += fmpp, fmpp := fmppTask.value)) ++
      Seq(
        libraryDependencies += (
          "net.sourceforge.fmpp" % "fmpp" % "0.9.16" % FmppConfig.name
        ),
        ivyConfigurations += FmppConfig,
        fullClasspath in FmppConfig := (
          update.map(
            _ select configurationFilter(FmppConfig.name) map Attributed.blank
          )
        ).value,
        // Add generated sources to sources JAR
        mappings in (Compile, packageSrc) ++= {
          val (base, srcs, srcDir) = (
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
    val (cp, r, output, s, srcDir) = (
      (fullClasspath in FmppConfig).value, (runner in fmpp).value,
      sourceManaged.value, streams.value, sourceDirectory.value
    )
    val fmppSrc = srcDir / "scala"
    val inFiles = (fmppSrc ** "*.fm" get).toSet
    val cachedFun =
      FileFunction.cached(s.cacheDirectory / "fmpp") { (in: Set[File]) =>
          IO.delete(output ** "*.scala" get)
          val args = List(
            "--expert", "-q", "-S", fmppSrc.getPath, "-O", output.getPath,
            "--replace-extensions=fm, scala", "-M", "execute(**/*.fm), ignore(**/*)"
          )
          r.run("fmpp.tools.CommandLine", cp.files, args, s.log).failed.foreach(
            sys error _.getMessage
          )
          (output ** "*.scala").get.toSet
      }
    cachedFun(inFiles).toSeq
  }
}

