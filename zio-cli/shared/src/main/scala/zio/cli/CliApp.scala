package zio.cli

import zio._
import zio.cli.figlet.FigFont

import zio.cli.HelpDoc.{h1, p}
import zio.cli.HelpDoc.Span.{code, text}
import zio.cli.BuiltInOption._
import zio.cli.completion.{Completion, CompletionScript}

import scala.annotation.tailrec
import zio.Console.printLine
import zio.System.envs

/**
 * A `CliApp[R, E]` is a complete description of a command-line application, which
 * requires environment `R`, and may fail with a value of type `E`.
 */
sealed trait CliApp[-R, +E, Model] {
  def run(args: List[String]): ZIO[R, Nothing, ExitCode]

  def config(newConfig: CliConfig): CliApp[R, E, Model]

  def footer(newFooter: HelpDoc): CliApp[R, E, Model]

  def summary(s: HelpDoc.Span): CliApp[R, E, Model]
}

object CliApp {

  def make[R, E, Model](
    name: String,
    version: String,
    summary: HelpDoc.Span,
    command: Command[Model],
    footer: HelpDoc = HelpDoc.Empty,
    config: CliConfig = CliConfig.default,
    figFont: FigFont = FigFont.Default
  )(
    execute: Model => ZIO[R, E, Any]
  ): CliApp[R, E, Model] =
    CliAppImpl(name, version, summary, command, execute, footer, config, figFont)

  private case class CliAppImpl[-R, +E, Model](
    name: String,
    version: String,
    summary: HelpDoc.Span,
    command: Command[Model],
    execute: Model => ZIO[R, E, Any],
    footer: HelpDoc = HelpDoc.Empty,
    config: CliConfig = CliConfig.default,
    figFont: FigFont = FigFont.Default
  ) extends CliApp[R, E, Model] { self =>
    def config(newConfig: CliConfig): CliApp[R, E, Model] = copy(config = newConfig)

    def executeBuiltIn(builtInOption: BuiltInOption): Task[Unit] = {
      def getHelpDescription(helpDoc: HelpDoc): HelpDoc.Span =
        helpDoc match {
          case HelpDoc.Header(value, _) => value
          case HelpDoc.Paragraph(value) => value
          case _                        => HelpDoc.Span.empty
        }

      builtInOption match {
        case ShowHelp(synopsis, helpDoc) =>
          val fancyName =
            p(code(figFont.render(command.names.headOption.getOrElse(name))))

          val header = p(text(name) + text(" v") + text(version) + text(" -- ") + summary)

          val synopsisHelpDoc = h1("usage") + HelpDoc.p(text("$ ") + getHelpDescription(synopsis.helpDoc))

          // TODO add rendering of built-in options such as help
          printLine((fancyName + header + synopsisHelpDoc + helpDoc + footer).toPlaintext(columnWidth = 120))

        case ShowCompletionScript(path, shellType) =>
          printLine(CompletionScript(path, if (command.names.nonEmpty) command.names else Set(name), shellType))
        case ShowCompletions(index, _) =>
          envs.flatMap { envMap =>
            val compWords = envMap.collect {
              case (idx, word) if idx.startsWith("COMP_WORD_") =>
                (idx.drop("COMP_WORD_".length).toInt, word)
            }.toList.sortBy(_._1).map(_._2)

            Completion
              .complete(compWords, index, command, config)
              .flatMap { completions =>
                ZIO.foreachDiscard(completions)(word => printLine(word))
              }
          }
      }
    }

    def footer(newFooter: HelpDoc): CliApp[R, E, Model] =
      copy(footer = self.footer + newFooter)

    def printDocs(helpDoc: HelpDoc): UIO[Unit] =
      printLine(helpDoc.toPlaintext(80)).!

    // prepend a first argument in case the CliApp's command is expected to consume it
    @tailrec
    private def prefix(command: Command[_]): List[String] =
      command match {
        case Command.Single(name, _, _, _)  => List(name)
        case Command.Map(command, _)        => prefix(command)
        case Command.OrElse(_, _)           => Nil
        case Command.Subcommands(parent, _) => prefix(parent)
      }

    def run(args: List[String]): ZIO[R, Nothing, ExitCode] =
      command
        .parse(prefix(command) ++ args, config)
        .foldZIO(
          e => printDocs(e.error),
          {
            case CommandDirective.UserDefined(_, value) => execute(value)
            case CommandDirective.BuiltIn(x)            => executeBuiltIn(x)
          }
        )
        .exitCode

    def summary(s: HelpDoc.Span): CliApp[R, E, Model] =
      copy(summary = self.summary + s)
  }
}
