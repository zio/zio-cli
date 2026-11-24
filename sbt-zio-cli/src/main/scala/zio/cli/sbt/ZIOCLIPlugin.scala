import sbt.Keys._
import sbtnativeimage.NativeImagePlugin
import zio.cli.sbt.ZIOCLIPluginKeys
import sbt.{Compile, Def, _}

/**
 * An sbt plugin that builds a native image of a ZIO CLI application. It requires `NativeImagePlugin` from
 * sbt-native-image, so enabling it also enables that plugin, and it defines the `zioCli*` keys of
 * `zio.cli.sbt.ZIOCLIPluginKeys`. It sets `nativeImageReady` and `nativeImageOptions` from `zioCliNativeImageReady` and
 * `zioCliNativeImageOptions`, and `zioCliBuildNative` runs `nativeImage`. The tasks `zioCliInstallCli`,
 * `zioCliGenerateBashCompletion` and `zioCliGenerateZshCompletion` only print a message that they are not implemented.
 * The plugin does not define `autoImport`, so import `ZIOCLIPlugin._` to use its keys in `build.sbt`.
 *
 * {{{
 * // no-check: needs an sbt build with the plugin
 * import ZIOCLIPlugin._
 *
 * lazy val cli = project
 *   .enablePlugins(ZIOCLIPlugin)
 *   .settings(
 *     Compile / mainClass := Some("example.Main"),
 *     zioCliNativeImageOptions += "--verbose"
 *   )
 * }}}
 *
 * Then `sbt cli/zioCliBuildNative` builds the native image.
 */
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
