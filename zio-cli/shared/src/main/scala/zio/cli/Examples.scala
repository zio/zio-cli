package zio.cli

import scala.language.postfixOps

// import java.nio.file.Path

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
  // .execute { (tuple: (Any, (Boolean, (Boolean, (Boolean, Boolean)))), tuple2: (Any, List[Path])) =>
  //   ???
  // }

  val wcApp = CLIApp("ZIO Word Count", "0.1.2", wc)

  object WcApp extends App {
    def run(args: List[String]) = wcApp.run(args)
  }
}
