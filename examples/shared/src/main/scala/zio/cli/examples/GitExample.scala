package zio.cli.examples

import zio.Console.printLine
import zio.cli.HelpDoc.Span.text
import zio.cli._

import java.nio.file.{Path => JPath}

object GitExample extends ZIOCliDefault {
  import java.nio.file.Path

  sealed trait Subcommand extends Product with Serializable
  object Subcommand {
    final case class Add(modified: Boolean, directory: JPath) extends Subcommand
    final case class Remote(verbose: Boolean)                 extends Subcommand
  }

  val modifiedFlag: Options[Boolean] = Options.boolean("m")

  val addHelp: HelpDoc = HelpDoc.p("Add subcommand description")
  val add =
    Command("add", modifiedFlag, Args.directory("directory")).withHelp(addHelp).map { case (modified, directory) =>
      Subcommand.Add(modified, directory)
    }

  val verboseFlag: Options[Boolean] = Options.boolean("verbose").alias("v")
  val configPath: Options[Path]     = Options.directory("c", Exists.Yes)

  val remoteHelp: HelpDoc = HelpDoc.p("Remote subcommand description")
  val remote = Command("remote", verboseFlag, Args.none).withHelp(remoteHelp).map { verbose =>
    Subcommand.Remote(verbose)
  }

  val git: Command[Subcommand] =
    Command("git", Options.none, Args.none).subcommands(add, remote)

  val cliApp = CliApp.make(
    name = "Git Version Control",
    version = "0.9.2",
    summary = text("a client for the git dvcs protocol"),
    command = git
  ) {
    case Subcommand.Add(modified, directory) =>
      printLine(s"Executing `git add $directory` with modified flag set to $modified")

    case Subcommand.Remote(verbose) =>
      printLine(s"Executing `git remote` with verbose flag set to $verbose")
  }
}
