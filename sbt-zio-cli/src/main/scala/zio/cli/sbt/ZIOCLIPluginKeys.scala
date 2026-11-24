package zio.cli.sbt

import sbt._

/**
 * The sbt keys of the ZIO CLI plugin: three settings (`zioCliMainClass`, `zioCliNativeImageOptions`,
 * `zioCliNativeImageReady`) and four tasks (`zioCliBuildNative`, `zioCliGenerateBashCompletion`,
 * `zioCliGenerateZshCompletion`, `zioCliInstallCli`). `ZIOCLIPlugin` mixes this trait in and gives the settings their
 * defaults.
 */
trait ZIOCLIPluginKeys {

  /**
   * The main class of the CLI application, as `Option[String]`. The plugin sets it to `None`. `zioCliBuildNative` reads
   * it in its body, where the body assigns `Compile / mainClass` inside the task.
   */
  lazy val zioCliMainClass = settingKey[Option[String]]("The mainClass of the CLI App in the Compile scope")

  /**
   * The arguments passed to the native-image builder, as `Seq[String]`. The plugin sets it to
   * `--allow-incomplete-classpath`, `--report-unsupported-elements-at-runtime`, `--initialize-at-build-time` and
   * `--no-fallback`, and passes it on as `nativeImageOptions` of sbt-native-image.
   */
  lazy val zioCliNativeImageOptions = settingKey[Seq[String]](
    "A collection of arguments to pass the native-image builder to customize native image generation"
  )

  /**
   * A callback, `() => Unit`, that sbt-native-image calls when the native image is ready (the plugin passes it on as
   * `nativeImageReady`). The plugin sets it to print `ZIO CLI App Native Image Ready!`.
   */
  lazy val zioCliNativeImageReady =
    settingKey[() => Unit]("A side-effecting callback that is called the native image is ready.")

  /** Builds the native image of the CLI application by running the `nativeImage` task of sbt-native-image. */
  lazy val zioCliBuildNative = taskKey[Unit]("Build a native image version of the CLI App")

  /**
   * Generates the bash completion for the CLI application. The task is not implemented: it prints a message and does
   * nothing else.
   */
  lazy val zioCliGenerateBashCompletion = taskKey[Unit]("Generate bash completion for the CLI App")

  /**
   * Generates the zsh completion for the CLI application. The task is not implemented: it prints a message and does
   * nothing else.
   */
  lazy val zioCliGenerateZshCompletion = taskKey[Unit]("Generate zsh completion for the CLI App")

  /**
   * Runs the universal installer for the CLI application. The task is not implemented: it prints a message and does
   * nothing else.
   */
  lazy val zioCliInstallCli = taskKey[Unit]("Run the universal installer")

}
