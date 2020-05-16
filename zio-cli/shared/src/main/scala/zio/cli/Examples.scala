package zio.cli

import scala.language.postfixOps

object Examples {

  /**
   *
   * wc [-clmw] [file ...]
   */
  val bytesFlag: Options[Boolean] = Options.bool("c", true)
  val linesFlag: Options[Boolean] = Options.bool("l", true)
  val wordsFlag: Options[Boolean] = Options.bool("w", true)
  val charFlag: Options[Boolean]  = Options.bool("m", false)

  val options = bytesFlag :: linesFlag :: wordsFlag :: charFlag

  val args = Args.path("files") *

  val wc = Command("wc")
    .options(options)
    .args(args)

  val wcApp = CLI("ZIO Word Count", "0.1.2", wc)

  object WcApp extends App {
    def run(args: List[String]) = wcApp.run(args)
  }
}
