package zio.cli

import zio.ZIOAppDefault

/**
 * A `ZIOAppDefault` that runs a [[CliApp]] as the program's main: define `cliApp`, and the program's command-line arguments are passed to its `run`. On a parse error, the error is printed and the program exits with code 1; a failure of the application's function is logged and the program exits with code 1 as well.
 *
 * {{{
 * import zio._
 *
 * object Greeter extends ZIOCliDefault {
 *   val greet = Command("greet", Args.text("name"))
 *
 *   val cliApp = CliApp.make("greeter", "1.0.0", HelpDoc.Span.text("Says hello."), greet) { name =>
 *     Console.printLine("Hello, " + name + "!")
 *   }
 * }
 * }}}
 *
 * @see [[https://zio.dev/zio-cli/ ZIO CLI]]
 */
trait ZIOCliDefault extends ZIOCli with ZIOAppDefault
