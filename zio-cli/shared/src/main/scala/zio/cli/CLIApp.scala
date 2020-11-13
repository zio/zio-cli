package zio.cli

import zio._
import zio.console._

import zio.cli.HelpDoc.{ h1, p }
import zio.cli.HelpDoc.Span.text

/**
 * A `CLIApp[R, E]` is a complete description of a command-line application, which
 * requires environment `R`, and may fail with a value of type `E`.
 */
final case class CLIApp[-R, +E, Model](
  name: String,
  version: String,
  summary: HelpDoc.Span,
  command: Command[Model],
  execute: Model => ZIO[R, E, Any],
  footer: HelpDoc = HelpDoc.Empty,
  options: ParserOptions = ParserOptions.default
) { self =>
  def completions(shellType: ShellType): String = ???

  final def footer(f: HelpDoc): CLIApp[R, E, Model] =
    copy(footer = self.footer + f)

  final def helpDoc: HelpDoc =
    h1(text(name) + text(" ") + text(version)) +
      p(text(name) + text(" -- ") + summary) +
      h1("synopsis") +
      command.synopsis.helpDoc +
      command.helpDoc +
      footer

  final def run(args: List[String]): ZIO[R with Console, Nothing, ExitCode] =
    builtInCommands(args) orElse (for {
      validationResult <- command.parse(args, options)
      (_, a)           = validationResult
      result           <- execute(a)
    } yield result).exitCode

  final def builtInCommands(args: List[String]): ZIO[Console, None.type, ExitCode] =
    if (args.length == 0 || args.headOption.map(_.toLowerCase) == Some("--help"))
      putStrLn(command.helpDoc.toPlaintext()).exitCode
    else ZIO.fail(None)

}
