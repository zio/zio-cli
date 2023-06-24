package zio.cli

import zio.Console.{print, printLine, readLine}
import zio.System.envs
import zio._
import zio.cli.BuiltInOption._
import zio.cli.HelpDoc.Span.{code, text}
import zio.cli.HelpDoc.{h1, p}
import zio.cli.completion.{Completion, CompletionScript}
import zio.cli.figlet.FigFont

import scala.annotation.tailrec

/**
 * A `CliApp[R, E]` is a complete description of a command-line application, which requires environment `R`, and may
 * fail with a value of type `E`.
 */
sealed trait CliApp[-R, +E, +Model] {
  def run(args: List[String]): ZIO[R, Any, Any]

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
  )(execute: Model => ZIO[R, E, Any]): CliApp[R, E, Model] =
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

    def footer(newFooter: HelpDoc): CliApp[R, E, Model] =
      copy(footer = self.footer + newFooter)

    def printDocs(helpDoc: HelpDoc): UIO[Unit] =
      printLine(helpDoc.toPlaintext(80)).!

    def run(args: List[String]): ZIO[R, Any, Any] = {
      def executeBuiltIn(builtInOption: BuiltInOption): ZIO[R, Any, Any] =
        builtInOption match {
          case ShowHelp(synopsis, helpDoc) =>
            val fancyName = p(code(self.figFont.render(self.name)))

            val header = p(text(self.name) + text(self.version) + text(" -- ") + self.summary)

            val synopsisHelpDoc = h1("usage") + HelpDoc.p(text("$ ") + synopsis.helpDoc.getSpan)

            // TODO add rendering of built-in options such as help
            printLine((fancyName + header + synopsisHelpDoc + helpDoc + self.footer).toPlaintext(columnWidth = 300))

          case ShowCompletionScript(path, shellType) =>
            printLine(
              CompletionScript(path, if (self.command.names.nonEmpty) self.command.names else Set(self.name), shellType)
            )
          case ShowCompletions(index, _) =>
            envs.flatMap { envMap =>
              val compWords = envMap.collect {
                case (idx, word) if idx.startsWith("COMP_WORD_") =>
                  (idx.drop("COMP_WORD_".length).toInt, word)
              }.toList.sortBy(_._1).map(_._2)

              Completion
                .complete(compWords, index, self.command, self.config)
                .flatMap { completions =>
                  ZIO.foreachDiscard(completions)(word => printLine(word))
                }
            }
          case Wizard(command) =>
            val subcommands = command.getSubcommands

            for {
              subcommandName <- if (subcommands.size == 1) ZIO.succeed(subcommands.keys.head)
                                else
                                  (print("Command" + subcommands.keys.mkString("(", "|", "): ")) *> readLine).orDie
              subcommand <-
                ZIO
                  .fromOption(subcommands.get(subcommandName))
                  .orElseFail(ValidationError(ValidationErrorType.InvalidValue, HelpDoc.p("Invalid subcommand")))
              args   <- subcommand.generateArgs
              _      <- Console.printLine(s"Executing command: ${(prefix(self.command) ++ args).mkString(" ")}")
              result <- self.run(args)
            } yield result
        }

      // prepend a first argument in case the CliApp's command is expected to consume it
      @tailrec
      def prefix(command: Command[_]): List[String] =
        command match {
          case Command.Single(name, _, _, _)  => List(name)
          case Command.Map(command, _)        => prefix(command)
          case Command.OrElse(_, _)           => Nil
          case Command.Subcommands(parent, _) => prefix(parent)
        }

      self.command
        .parse(prefix(self.command) ++ args, self.config)
        .foldZIO(
          e => printDocs(e.error) *> ZIO.fail(e),
          {
            case CommandDirective.UserDefined(_, value) => self.execute(value)
            case CommandDirective.BuiltIn(x) =>
              executeBuiltIn(x).catchSome { case e: ValidationError =>
                printDocs(e.error) *> ZIO.fail(e)
              }
          }
        )
    }

    def summary(s: HelpDoc.Span): CliApp[R, E, Model] =
      copy(summary = self.summary + s)
  }
}
