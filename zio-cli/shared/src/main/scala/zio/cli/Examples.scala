package zio.cli

import scala.language.postfixOps

trait WordCountExample {

  /**
   * wc [-clmw] [file ...]
   */
  val bytesFlag: Options[Boolean] = Options.bool("c", true)
  val linesFlag: Options[Boolean] = Options.bool("l", true)
  val wordsFlag: Options[Boolean] = Options.bool("w", true)
  val charFlag: Options[Boolean]  = Options.bool("m", false)

  case class WcOptions(bytes: Boolean, lines: Boolean, words: Boolean, char: Boolean)

  val options = (bytesFlag :: linesFlag :: wordsFlag :: charFlag).as(WcOptions)

  val args = Args.file("files", true) *

  val wc = Command("wc")
    .options(options)
    .args(args)

  val wcApp = CLIApp("ZIO Word Count", "0.1.2", wc)

  object WcApp extends App {
    def run(args: List[String]) = wcApp.run(args)
  }
}

trait GitExample {
  import java.nio.file.Path

  val verboseFlag: Options[Boolean] = Options.bool("v", true)

  val configPath: Options[Path] = Options.directory("c", true)

  val modifiedFlag: Options[Boolean] = Options.bool("m", true)

  // git remote [-v | --verbose] show [-n] <name>...
  // git remote [-v | --verbose] update [-p | --prune] [(<group> | <remote>)...]

  sealed trait Subcommand
  object Subcommand {
    sealed case class Add()    extends Subcommand
    sealed case class Remote() extends Subcommand
  }

  val add = Command("add")
    .options(modifiedFlag)
    .args(Args.directory("directory", true))

  val remote = Command("remote")
    .options(verboseFlag)

  // Command[Subcommands, Env, Error]
  val git = Command("git")
    .options(configPath)
  // .subcommand(remote)
  // .subcommand(add)
  // .execute {
  //   case (Parent, Remote()) =>
  //   case (Parent, Add()) =>
  //   ...
  // }
  //
}
