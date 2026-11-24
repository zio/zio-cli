package zio.cli

import zio.Console.printLine
import zio.System.envs
import zio._
import zio.cli.BuiltInOption._
import zio.cli.HelpDoc.Span.{code, text}
import zio.cli.HelpDoc.{h1, p}
import zio.cli.completion.{Completion, CompletionScript}
import zio.cli.figlet.FigFont

import scala.annotation.tailrec

/**
 * A `CliApp[R, E, A]` is a complete command-line application: a name, a version, a summary, the [[Command]] that
 * describes what the user may type, and the function that runs once the input is parsed. Running it needs an
 * environment `R`, can fail with an error `E`, and produces a value `A`.
 *
 * Create one with `CliApp.make` and run it with `run`, or define it as the `cliApp` of a [[ZIOCliDefault]] program.
 * `--help` shows a help screen built from the name, version, summary, command and footer.
 *
 * {{{
 * import zio._
 *
 * val greet = Command("greet", Options.text("name"))
 *
 * val app: CliApp[Any, java.io.IOException, Unit] =
 *   CliApp.make("greeter", "1.0.0", HelpDoc.Span.text("Says hello."), greet) { name =>
 *     Console.printLine("Hello, " + name + "!")
 *   }
 * }}}
 *
 * @see
 *   [[https://zio.dev/zio-cli/ ZIO CLI]]
 */
sealed trait CliApp[-R, +E, +A] { self =>

  /**
   * Parses `args`, the words typed after the program's name, and runs the application. For a single command, or a
   * command with subcommands, the command's own name is added in front of `args`, so users don't type it; for
   * alternatives built with `|`, the first word picks the command.
   *
   * Returns `Some(result)` after running the application's function, or `None` after a built-in option such as
   * `--help`. Words left over after the command's arguments are ignored. On a parse error, it prints the error to the
   * console and fails with `CliError.Parsing`; a failure of the application's function becomes `CliError.Execution`.
   */
  def run(args: List[String]): ZIO[R, CliError[E], Option[A]]

  /**
   * Returns a copy that parses the command line with `newConfig`.
   */
  def config(newConfig: CliConfig): CliApp[R, E, A]

  /**
   * Transforms the result of the application with `f`.
   */
  final def map[B](f: A => B): CliApp[R, E, B] =
    self match {
      case CliApp.CliAppImpl(name, version, summary, command, execute, footer, config, figFont) =>
        CliApp.CliAppImpl(name, version, summary, command, execute.andThen(_.map(f)), footer, config, figFont)
    }

  /**
   * Runs `f` after the application's function, with its result. A failure of `f` becomes a `CliError.Execution`.
   */
  def flatMap[R1 <: R, E1 >: E, B](f: A => ZIO[R1, E1, B]): CliApp[R1, E1, B]

  /**
   * Adds `newFooter` after the current footer; the footer ends the help screen.
   */
  def footer(newFooter: HelpDoc): CliApp[R, E, A]

  /**
   * Appends `s` to the current summary, which the help screen shows after the name and version.
   */
  def summary(s: HelpDoc.Span): CliApp[R, E, A]

}

/**
 * The constructor for [[CliApp]].
 */
object CliApp {

  /**
   * Creates an application. The help screen starts with `name` drawn as a banner in `figFont`, then `name`, `version`
   * and `summary` on one line, and ends with `footer`. `config` sets how the command line is parsed. Once `command` has
   * parsed the command line, `execute` runs with its value.
   */
  def make[R, E, Model, A](
    name: String,
    version: String,
    summary: HelpDoc.Span,
    command: Command[Model],
    footer: HelpDoc = HelpDoc.Empty,
    config: CliConfig = CliConfig.default,
    figFont: FigFont = FigFont.Default
  )(execute: Model => ZIO[R, E, A]): CliApp[R, E, A] =
    CliAppImpl(name, version, summary, command, execute, footer, config, figFont)

  private[cli] case class CliAppImpl[-R, +E, Model, +A](
    name: String,
    version: String,
    summary: HelpDoc.Span,
    command: Command[Model],
    execute: Model => ZIO[R, E, A],
    footer: HelpDoc = HelpDoc.Empty,
    config: CliConfig = CliConfig.default,
    figFont: FigFont = FigFont.Default
  ) extends CliApp[R, E, A] { self =>
    def config(newConfig: CliConfig): CliApp[R, E, A] = copy(config = newConfig)

    def footer(newFooter: HelpDoc): CliApp[R, E, A] =
      copy(footer = self.footer + newFooter)

    private def printDocs(helpDoc: HelpDoc): UIO[Unit] =
      printLine(helpDoc.toPlaintext(80)).!

    def run(args: List[String]): ZIO[R, CliError[E], Option[A]] = {
      def executeBuiltIn(builtInOption: BuiltInOption): ZIO[R, CliError[E], Option[A]] =
        builtInOption match {
          case ShowHelp(synopsis, helpDoc) =>
            val fancyName = p(code(self.figFont.render(self.name)))

            val header = p(text(self.name) + text(" ") + text(self.version) + text(" -- ") + self.summary)

            val synopsisHelpDoc = h1("usage") + synopsis
              .enumerate(config)
              .map(span => text("$ ") + span)
              .map(HelpDoc.p)
              .foldRight(HelpDoc.empty)(_ + _)

            // TODO add rendering of built-in options such as help
            printLine(
              (fancyName + header + synopsisHelpDoc + helpDoc + self.footer).toPlaintext(columnWidth = 300)
            ).mapBoth(CliError.IO(_), _ => None)
          case ShowCompletionScript(path, shellType) =>
            printLine(
              CompletionScript(path, if (self.command.names.nonEmpty) self.command.names else Set(self.name), shellType)
            ).mapBoth(CliError.IO(_), _ => None)
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
            }.mapBoth(CliError.BuiltIn(_), _ => None)
          case ShowWizard(command) =>
            val fancyName   = p(code(self.figFont.render(self.name)))
            val header      = p(text("WIZARD of ") + text(self.name) + text(self.version) + text(" -- ") + self.summary)
            val explanation = p(s"Wizard mode assist you in constructing commands for $name$version")

            (for {
              parameters <-
                Wizard(command, config, fancyName + header + explanation).execute.mapError(CliError.BuiltIn(_))
              output <- run(parameters)
            } yield output).catchSome { case CliError.BuiltIn(_) =>
              ZIO.none
            }
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
          e => printDocs(e.error) *> ZIO.fail(CliError.Parsing(e)),
          {
            case CommandDirective.UserDefined(_, value) =>
              self.execute(value).mapBoth(CliError.Execution(_), Some(_))
            case CommandDirective.BuiltIn(x) =>
              executeBuiltIn(x).catchSome { case err @ CliError.Parsing(e) =>
                printDocs(e.error) *> ZIO.fail(err)
              }
          }
        )
    }

    override def flatMap[R1 <: R, E1 >: E, B](f: A => ZIO[R1, E1, B]): CliApp[R1, E1, B] =
      CliAppImpl[R1, E1, Model, B](
        name,
        version,
        summary,
        command,
        { (app: ZIO[R, E, A]) => app.flatMap(f) } compose execute,
        footer,
        config,
        figFont
      )

    override def summary(s: HelpDoc.Span): CliApp[R, E, A] =
      copy(summary = self.summary + s)
  }
}
