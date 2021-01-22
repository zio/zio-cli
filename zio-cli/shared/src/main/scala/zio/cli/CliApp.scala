package zio.cli

import zio._
import zio.cli.figlet.FigFont
import zio.console._
import zio.cli.HelpDoc.{ h1, p }
import zio.cli.HelpDoc.Span.{ code, text }
import zio.cli.BuiltInOption._

/**
 * A `CliApp[R, E]` is a complete description of a command-line application, which
 * requires environment `R`, and may fail with a value of type `E`.
 */
final case class CliApp[-R, +E, Model](
  name: String,
  version: String,
  summary: HelpDoc.Span,
  command: Command[Model],
  execute: Model => ZIO[R, E, Any],
  footer: HelpDoc = HelpDoc.Empty,
  config: CliConfig = CliConfig.default,
  figFont: FigFont = FigFont.Default
) { self =>
  def config(newConfig: CliConfig): CliApp[R, E, Model] = copy(config = newConfig)

  def executeBuiltIn(builtInOption: BuiltInOption): RIO[Console, Unit] =
    builtInOption match {
      case ShowHelp(helpDoc) =>
        putStrLn(helpDoc.toPlaintext(80))

      case ShowCompletions(completions) =>
        putStrLn(completions.map(_.mkString(" ")).mkString("\n"))
    }

  def footer(newFooter: HelpDoc): CliApp[R, E, Model] =
    copy(footer = self.footer + newFooter)

  def helpDoc: HelpDoc =
    p(code(figFont.render(command.names.headOption.getOrElse(name)))) +
      p(text(name) + text(" ") + text(version) + text(" -- ") + summary) +
      h1("synopsis") +
      command.synopsis.helpDoc +
      command.helpDoc +
      footer

  def printDocs(helpDoc: HelpDoc): URIO[Console, Unit] =
    putStrLn(helpDoc.toPlaintext(80))

  def run(args: List[String]): ZIO[R with Console, Nothing, ExitCode] =
    command
      .parse(args, config)
      .foldM(printDocs(_), {
        case CommandDirective.UserDefined(_, value) => execute(value)

        case CommandDirective.BuiltIn(x) => executeBuiltIn(x)
      })
      .exitCode

  def summary(s: HelpDoc.Span): CliApp[R, E, Model] =
    copy(summary = self.summary + s)
}
