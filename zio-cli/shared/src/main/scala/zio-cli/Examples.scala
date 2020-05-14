package zio.cli

import scala.language.postfixOps

object Examples {

  /**
   *
   * wc [-clmw] [file ...]
   */

  val bytesFlag: Flags[Boolean] = Flags.bool("c", true)
  val linesFlag: Flags[Boolean] = Flags.bool("l", true)
  val wordsFlag: Flags[Boolean] = Flags.bool("w", true)
  val charFlag : Flags[Boolean] = Flags.bool("m", false)

  val flags = bytesFlag :: linesFlag :: wordsFlag :: charFlag

  val args = Args.path*

  val wc = Command("wc")
    .flags(flags)
    .args(args)

}
