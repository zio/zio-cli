package zio.cli.examples

import java.nio.file.{ Path => JPath }

import zio.cli.{ Args, CliApp, Command, Exists, Options }
import zio.cli.HelpDoc.Span.text

import zio._
import zio.console.putStrLn

object GitExample extends App {
  import java.nio.file.Path

  val verboseFlag: Options[Boolean] = Options.bool("v", true)

  val configPath: Options[Path] = Options.directory("c", Exists.Yes)

  val modifiedFlag: Options[Boolean] = Options.bool("m", true)

  sealed trait Subcommand extends Product with Serializable
  object Subcommand {
    final case class Add(modified: Boolean, directory: JPath) extends Subcommand
    final case class Remote(verbose: Boolean)                 extends Subcommand
  }

  val add =
    Command("add", modifiedFlag, Args.directory("directory", Exists.Yes)).map {
      case (modified, directory) => Subcommand.Add(modified, directory)
    }

  val remote = Command("remote", verboseFlag, Args.none).map {
    case (verbose, _) => Subcommand.Remote(verbose)
  }

  // Command[Subcommands, Env, Error]
  val git: Command[Subcommand] =
    Command("git", Options.none, Args.none).subcommands(add | remote).map(_._2)

  val gitApp = CliApp(
    "Git Version Control",
    "0.9.2",
    text("a client for the git dvcs protocol"),
    git,
    (c: Subcommand) => putStrLn(c.toString())
  )

  override def run(args: List[String]) = gitApp.run(args)
}
