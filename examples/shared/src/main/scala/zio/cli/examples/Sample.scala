package zio.cli.examples

import zio.cli.{CliApp, ZIOCliDefault}
import zio.cli._
import zio.cli.HelpDoc.Span.text
import zio.Console.printLine

object Sample extends ZIOCliDefault {

  val options: Options[Either[BigInt, String]] = Options.integer("opt1").orElseEither(Options.text("opt2"))
  val arguments: Args[String] = Args.text("repository")
  val help: HelpDoc = HelpDoc.p("Creates a copy of an existing repository")

  val command: Command[(Either[BigInt, String], String)] = Command("clone").subcommands(Command("clone", options, arguments).withHelp(help))

  val cliApp = CliApp.make(
    name = "Sample Git",
    version = "1.1.0",
    summary = text("Sample implementation of git clone"),
    command = command
  ) {
    // Implement logic of CliApp
    case _ => printLine("executing git clone")
  }

}
