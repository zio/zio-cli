package zio.cli

import zio.{ExitCode, Scope, ZIOApp, ZIOAppArgs}

/**
 * A `ZIOApp` whose `run` passes the program's command-line arguments to `cliApp`. On `CliError.Parsing`, it exits with
 * code 1 (`CliApp#run` has already printed the error); other failures are left to `ZIOApp`. Extend [[ZIOCliDefault]] to
 * get the default runtime as well.
 */
trait ZIOCli extends ZIOApp {

  /**
   * The application that runs with the program's command-line arguments.
   */
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
