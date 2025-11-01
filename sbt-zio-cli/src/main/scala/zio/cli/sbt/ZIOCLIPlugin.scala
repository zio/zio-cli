import sbt.Keys._
import sbtnativeimage.NativeImagePlugin
import zio.cli.sbt.ZIOCLIPluginKeys
import sbt.{Compile, Def, _}

object ZIOCLIPlugin extends AutoPlugin with ZIOCLIPluginKeys {
  override def requires = NativeImagePlugin

  override def projectSettings: Seq[Def.Setting[_]] = Seq(
    zioCliNativeImageReady := { () =>
      println("ZIO CLI App Native Image Ready!")
    },
    zioCliNativeImageOptions := List(
      "--allow-incomplete-classpath",
      "--report-unsupported-elements-at-runtime",
      "--initialize-at-build-time",
      "--no-fallback"
    ),
    zioCliMainClass                        := None,
    requires.autoImport.nativeImageReady   := zioCliNativeImageReady.value,
    requires.autoImport.nativeImageOptions := zioCliNativeImageOptions.value,
    zioCliBuildNative                      := {
      Compile / mainClass := {
        zioCliMainClass.value
      }
      requires.autoImport.nativeImage.value
    },
    zioCliInstallCli := {
      println("TODO: Not Implemented!")
    },
    zioCliGenerateBashCompletion := {
      println("TODO: Not Implemented!")
    },
    zioCliGenerateZshCompletion := {
      println("TODO: Not Implemented!")
    }
  )

}
