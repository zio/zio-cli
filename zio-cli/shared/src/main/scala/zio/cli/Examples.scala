package zio.cli

import java.nio.file.Path

import zio.App
import zio.URIO
import zio.console.Console

import zio.cli.HelpDoc.Span.text

object WcApp extends App {

  val bytesFlag: Options[Boolean] = Options.bool("c", true).alias("c")
  val linesFlag: Options[Boolean] = Options.bool("l", true).alias("l")
  val wordsFlag: Options[Boolean] = Options.bool("w", true).alias("w")
  val charFlag: Options[Boolean]  = Options.bool("m", false).alias("m")

  case class WcOptions(bytes: Boolean, lines: Boolean, words: Boolean, char: Boolean)

  val options = (bytesFlag :: linesFlag :: wordsFlag :: charFlag).as(WcOptions)

  val args = Args.file(Exists.Yes).repeat1

  val wc = Command("wc", options, args)

  val execute: (WcOptions, ::[Path]) => URIO[Console, Unit] = (opts, paths) => zio.console.putStrLn(s"${opts} ${paths}")

  val wcApp = CLIApp(
    "ZIO Word Count",
    "0.1.2",
    text("counts words in the file"),
    wc,
    execute.tupled
  )

  @Override
  def run(args: List[String]) = wcApp.run(args)
}

trait GitExample {
  import java.nio.file.Path

  val verboseFlag: Options[Boolean] = Options.bool("v", true)

  val configPath: Options[Path] = Options.directory("c", Exists.Yes)

  val modifiedFlag: Options[Boolean] = Options.bool("m", true)

  // git remote [-v | --verbose] show [-n] <name>...
  // git remote [-v | --verbose] update [-p | --prune] [(<group> | <remote>)...]

  sealed trait Subcommand
  object Subcommand {
    sealed case class Add()    extends Subcommand
    sealed case class Remote() extends Subcommand
  }

  val add = Command("add", modifiedFlag, Args.directory("directory", Exists.Yes))

  val remote = Command("remote", verboseFlag, Args.none)

  // Command[Subcommands, Env, Error]
  val git = Command("git", configPath, Args.none)
  // .subcommand(remote)
  // .subcommand(add)
  // .execute {
  //   case (Parent, Remote()) =>
  //   case (Parent, Add()) =>
  //   ...
  // }
  //
}
