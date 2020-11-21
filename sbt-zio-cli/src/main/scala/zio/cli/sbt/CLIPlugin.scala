import sbt.Keys._
import sbtnativeimage.NativeImagePlugin
import zio.cli.sbt.CLIPluginKeys
import sbt.{Compile, Def, _}

object CLIPlugin extends AutoPlugin with CLIPluginKeys {
  override def requires = NativeImagePlugin

  def alertWhenDone(): Unit = {
    // The default uses an audio narrator, so let's print a message instead
    println("ZIO CLI App Native Image Ready!")
  }

  override def projectSettings: Seq[Def.Setting[_]] = Seq(
    // Defaults
    zioCliNativeImageOptions := List(
      "--allow-incomplete-classpath",
      "--report-unsupported-elements-at-runtime",
      "--initialize-at-build-time",
      "--no-fallback",
    ),
    zioCliMainClass := None, // Default to None, will require being set. Hook into sbt's default main?
    Compile / mainClass := {
      zioCliMainClass.value
    },
    requires.autoImport.nativeImageReady := alertWhenDone,
    requires.autoImport.nativeImageOptions := {
      zioCliNativeImageOptions.value
    },
    // Currently just piggyback off of the default nativeImage
    zioCliBuildNative := requires.autoImport.nativeImage.value,
    zioCliInstallCli := {
      println("TODO")
    },
    zioCliGenerateBashCompletion := {
      println("TODO")
    },
    zioCliGenerateZshCompletion := {
      println("TODO")
    }

  )

}
