package zio.cli

import zio.{Scope, ZIOApp, ZIOAppArgs}

trait ZIOCli extends ZIOApp {

  def cliApp: CliApp[Environment with ZIOAppArgs with Scope, Any, Any]

  override def run =
    for {
      args   <- ZIOAppArgs.getArgs
      result <- cliApp.run(args.toList)
    } yield result
}
