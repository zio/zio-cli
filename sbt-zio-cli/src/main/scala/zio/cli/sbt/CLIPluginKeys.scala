package zio.cli.sbt

import sbt._

trait CLIPluginKeys {

  // prefix with zioCli for now
  lazy val zioCliMainClass = settingKey[Option[String]]("The mainClass of the CLI App")
  lazy val zioCliNativeImageOptions = settingKey[Seq[String]]("The mainClass of the CLI App")

  lazy val zioCliBuildNative = taskKey[Unit]("Build a native image version of the CLI App")
  lazy val zioCliGenerateBashCompletion = taskKey[Unit]("Generate bash completion for the CLIP App")
  lazy val zioCliGenerateZshCompletion = taskKey[Unit]("Generate zsh completion for the CLIP App")
  lazy val zioCliInstallCli = taskKey[Unit]("Run the universal installer")

}
