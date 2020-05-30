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

  val options = bytesFlag :: linesFlag :: wordsFlag :: charFlag

  val args = Args.file("files", true) *

  val wc = Command("wc")
    .options(options)
    .args(args)
  // we provide no way to configure what execution takes place

  val wcApp = CLIApp("ZIO Word Count", "0.1.2", wc)

  object WcApp extends App {
    def run(args: List[String]) = wcApp.run(args)
  }
}
