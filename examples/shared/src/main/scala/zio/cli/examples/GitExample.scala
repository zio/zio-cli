package zio.cli.examples

import zio.cli.{ Args, Command, Exists, Options }

trait GitExample {
  import java.nio.file.Path

  val verboseFlag: Options[Boolean] = Options.bool("v", true)

  val configPath: Options[Path] = Options.directory("c", Exists.Yes)

  val modifiedFlag: Options[Boolean] = Options.bool("m", true)

  // git remote [-v | --verbose] show [-n] <name>...
  // git remote [-v | --verbose] update [-p | --prune] [(<group> | <remote>)...]

  sealed trait Subcommand
  object Subcommand {
    sealed case class Add()    extends Subcommand
    sealed case class Remote() extends Subcommand
  }
  // git add --help
  // git --help
  //

  val add = Command("add", modifiedFlag, Args.directory("directory", Exists.Yes))

  val remote = Command("remote", verboseFlag, Args.none)

  // Command[Subcommands, Env, Error]
  val git = Command("git", configPath, Args.none)
  // .subcommand(remote)
  // .subcommand(add)
  // .execute {
  //   case (Parent, Remote()) =>
  //   case (Parent, Add()) =>
  //   ...
  // }
  //
}
