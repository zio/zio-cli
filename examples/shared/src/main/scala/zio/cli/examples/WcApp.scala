package zio.cli.examples

import java.nio.file.Path

import zio.cli.HelpDoc.Span.text
import zio.cli._
import zio.console.Console
import zio.{ App, URIO }

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

  val wcApp = CliApp(
    "ZIO Word Count",
    "0.1.2",
    text("counts words in the file"),
    wc,
    execute.tupled
  )

  @Override
  def run(args: List[String]) = wcApp.run(args)
}
