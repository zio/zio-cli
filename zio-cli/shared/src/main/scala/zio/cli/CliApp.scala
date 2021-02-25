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
        val fancyName =
          p(code(figFont.render(command.names.headOption.getOrElse(name))))

        val synopsis = h1("synopsis") +
          command.synopsis.helpDoc

        val header = p(text(name) + text(" ") + text(version) + text(" -- ") + summary)

        putStrLn((header + fancyName + synopsis + helpDoc + footer).toPlaintext(80))

      case ShowCompletions(completions) =>
        putStrLn(completions.map(_.mkString(" ")).mkString("\n"))
    }

  def footer(newFooter: HelpDoc): CliApp[R, E, Model] =
    copy(footer = self.footer + newFooter)

  def printDocs(helpDoc: HelpDoc): URIO[Console, Unit] =
    putStrLn(helpDoc.toPlaintext(80))

  // prepend a first argument in case the CliApp's command is expected to consume it
  private def prefix(command: Command[_]): List[String] =
    command match {
      case Command.Single(name, _, _, _)  => List(name)
      case Command.Map(command, _)        => prefix(command)
      case Command.Fallback(_, _)         => Nil
      case Command.Subcommands(parent, _) => prefix(parent)
    }

  def run(args: List[String]): ZIO[R with Console, Nothing, ExitCode] =
    command
      .parse(prefix(command) ++ args, config)
      .foldM(e => printDocs(e.error), {
        case CommandDirective.UserDefined(_, value) => execute(value)
        case CommandDirective.BuiltIn(x)            => executeBuiltIn(x)
      })
      .exitCode

  def summary(s: HelpDoc.Span): CliApp[R, E, Model] =
    copy(summary = self.summary + s)
}
