package zio.cli.sbt

import sbt._

trait ZIOCLIPluginKeys {

  lazy val zioCliMainClass          = settingKey[Option[String]]("The mainClass of the CLI App in the Compile scope")
  lazy val zioCliNativeImageOptions = settingKey[Seq[String]](
    "A collection of arguments to pass the native-image builder to customize native image generation"
  )
  lazy val zioCliNativeImageReady =
    settingKey[() => Unit]("A side-effecting callback that is called the native image is ready.")

  lazy val zioCliBuildNative            = taskKey[Unit]("Build a native image version of the CLI App")
  lazy val zioCliGenerateBashCompletion = taskKey[Unit]("Generate bash completion for the CLI App")
  lazy val zioCliGenerateZshCompletion  = taskKey[Unit]("Generate zsh completion for the CLI App")
  lazy val zioCliInstallCli             = taskKey[Unit]("Run the universal installer")

}
