package zio.cli

import zio.{ExitCode, Scope, ZIOApp, ZIOAppArgs}

trait ZIOCli extends ZIOApp {

  def cliApp: CliApp[Environment with ZIOAppArgs with Scope, Any, Any]

  override def run =
    for {
      args   <- ZIOAppArgs.getArgs
      result <- cliApp.run(args.toList).catchSome[Environment with ZIOAppArgs with Scope, CliError[Any], Any] {
                  case CliError.Parsing(_) =>
                    // Validation errors are pretty printed by clipApp.run
                    exit(ExitCode.failure)
                }
    } yield result
}
