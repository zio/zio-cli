package zio.cli

import zio._
import zio.cli.Command.BuiltIn
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
  def handleBuiltIn(args: List[String], builtIn: BuiltIn): ZIO[Console, Nothing, Unit] =
    if (args.isEmpty || builtIn.help) printDocs(helpDoc)
    else
      builtIn.shellCompletions match {
        case None        => IO.unit
        case Some(value) => putStrLn(completions(value))
      }

  def completions(shellType: ShellType): String = ???

  def footer(f: HelpDoc): CLIApp[R, E, Model] =
    copy(footer = self.footer + f)

  def helpDoc: HelpDoc =
    h1(text(name) + text(" ") + text(version)) +
      p(text(name) + text(" -- ") + summary) +
      h1("synopsis") +
      command.synopsis.helpDoc +
      command.helpDoc +
      footer

  def options(o: ParserOptions): CLIApp[R, E, Model] =
    copy(options = o)

  def run(args: List[String]): ZIO[R with Console, Nothing, ExitCode] =
    (for {
      builtInValidationResult  <- command.parseBuiltIn(args, options)
      (remainingArgs, builtIn) = builtInValidationResult
      _                        <- handleBuiltIn(args, builtIn)
      validationResult         <- command.parse(remainingArgs, options)
    } yield validationResult)
      .foldM(printDocs, success => execute(success._2))
      .exitCode

  def printDocs(helpDoc: HelpDoc): URIO[Console, Unit] =
    putStrLn(helpDoc.toPlaintext(80))

  def summary(s: HelpDoc.Span): CLIApp[R, E, Model] =
    copy(summary = self.summary + s)
}
