package zio.cli

import java.nio.file.Path

import zio.App
import zio.URIO
import zio.console.Console

import scala.language.postfixOps

object WcApp extends App {
  @Override
  def run(args: List[String]) = WordCountExample.wcApp.run(args)
}

object WordCountExample {
  def main(args: Array[String]): Unit = {
    import HelpDoc.dsl

    val helpDoc = HelpDoc(
      dsl.blocks(
        dsl.h1("wc"),
        dsl.p("wc -- counts words in the file"),
        dsl.h1("synopsis"),
        dsl.p("cat [-benstuv] [file ...]"),
        dsl.h1("description"),
        dsl.p(
          "The cat utility reads files sequentially, writing them to the standard output. The file operands are processed in command-line order.  If file is a single dash (`-') or absent, cat reads from the standard input. If file is a UNIX domain socket, cat connects to it and then reads it until EOF.  This complements the UNIX domain binding capability available in inetd(8)."
        ),
        dsl.p("The options are as follows:"),
        dsl.descriptionList(
          dsl.text("-b") -> dsl.p("Number the non-blank output lines, starting at 1."),
          dsl.text("-d") -> dsl.p("Display non-printing characters (see the -v option)")
        )
      ) :: Nil
    )

    println(helpDoc.toPlaintext(80))
  }

  /**
   * wc [-clmw] [file ...]
   */
  val bytesFlag: Options[Boolean] = Options.bool("c", true).alias("c")
  val linesFlag: Options[Boolean] = Options.bool("l", true).alias("l")
  val wordsFlag: Options[Boolean] = Options.bool("w", true).alias("w")
  val charFlag: Options[Boolean]  = Options.bool("m", false).alias("m")

  case class WcOptions(bytes: Boolean, lines: Boolean, words: Boolean, char: Boolean)

  val options = (bytesFlag :: linesFlag :: wordsFlag :: charFlag).as(WcOptions)

  val args = Args.file("files", true) *

  val wc = Command("wc")
    .options(options)
    .args(args)

  val execute: (WcOptions, List[Path]) => URIO[Console, Unit] = (opts, paths) =>
    zio.console.putStrLn(s"${opts} ${paths}")

  val wcApp = CLIApp[Console, Nothing, (WcOptions, List[Path])]("ZIO Word Count", "0.1.2", wc, execute.tupled)
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
