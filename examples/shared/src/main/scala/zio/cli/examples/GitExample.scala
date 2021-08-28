package zio.cli.examples

import java.nio.file.{Path => JPath}

import zio.cli.{Args, CliApp, Command, Exists, Options}
import zio.cli.HelpDoc.Span.text

import zio._
import zio.console.putStrLn

object GitExample extends App {
  import java.nio.file.Path

  val verboseFlag: Options[Boolean] = Options.boolean("verbose").alias("v")

  val configPath: Options[Path] = Options.directory("c", Exists.Yes)

  val modifiedFlag: Options[Boolean] = Options.boolean("m")

  // zio-http
  // <*> -> (A, B)
  // <>
  // ++
  // A ++ B -> (A, B)
  val cool: Options[((Boolean, JPath), JPath)] =
    verboseFlag ++ configPath ++ configPath

  // git add ./oeusnh -m
  // git add -m ./oeusnh
  // git remote -v
  sealed trait Subcommand extends Product with Serializable
  object Subcommand {
    final case class Add(modified: Boolean, directory: JPath) extends Subcommand
    final case class Remote(verbose: Boolean)                 extends Subcommand
  }

  val add =
    Command("add", modifiedFlag, Args.directory("directory", Exists.Yes)).map { case (modified, directory) =>
      Subcommand.Add(modified, directory)
    }

  val remote = Command("remote", verboseFlag, Args.none).map { case (verbose, _) =>
    Subcommand.Remote(verbose)
  }

  // Command[Subcommands, Env, Error]
  val git: Command[Subcommand] =
    Command("git", Options.none, Args.none).subcommands(add | remote).map(_._2)

  val gitApp = CliApp(
    name = "Git Version Control",
    version = "0.9.2",
    summary = text("a client for the git dvcs protocol"),
    command = git,
    execute = (c: Subcommand) => putStrLn(c.toString())
  )

  override def run(args: List[String]) = gitApp.run(args)
}
