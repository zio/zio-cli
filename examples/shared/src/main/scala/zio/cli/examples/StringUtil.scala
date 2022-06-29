package zio.cli.examples

import zio._
import zio.cli.HelpDoc.Span.text
import zio.cli.HelpDoc.p
import zio.cli._

object StringUtil extends ZIOAppDefault {
  sealed trait Subcommand
  object Subcommand {
    final case class Split(string: String, first: Boolean, separator: String) extends Subcommand
    final case class Join(strings: NonEmptyChunk[String], separator: String)  extends Subcommand
  }

  val firstOption =
    Options.boolean(name = "first").alias("f") ?? "Display just the first substring."
  val separatorOption = Options.text("separator").alias("s").withDefault(",") ?? "Separator regex."
  val stringArg       = Args.text("string") ?? "String to split."

  val split =
    Command("split", firstOption ++ separatorOption, stringArg)
      .withHelp(p("Split a string into substrings and display as an array"))
      .map { case ((first, separator), string) =>
        Subcommand.Split(string, first, separator)
      }

  val join =
    Command("join", separatorOption, Args.text("string").+ ?? "Strings to join.")
      .withHelp(p("Join the command-arguments into a single string"))
      .map { case (separator, strings) =>
        Subcommand.Join(NonEmptyChunk.fromCons(strings), separator)
      }

  val stringUtil: Command[Subcommand] =
    Command("string-util", Options.none, Args.none).subcommands(split, join)

  val stringUtilApp = CliApp.make(
    name = "String Util",
    version = "0.0.1",
    summary = text("CLI to some string utilities"),
    footer = HelpDoc.p("©Copyright 2022"),
    command = stringUtil
  ) {
    case Subcommand.Split(string, first, separator) =>
      val elements = string.split(separator)
      Console.printLine(if (first) elements.headOption.getOrElse("") else elements.mkString("[", ", ", "]"))
    case Subcommand.Join(strings, separator) =>
      Console.printLine(strings.mkString(separator))
  }

  override def run =
    for {
      args <- ZIOAppArgs.getArgs
      _    <- stringUtilApp.run(args.toList)
    } yield ()
}
