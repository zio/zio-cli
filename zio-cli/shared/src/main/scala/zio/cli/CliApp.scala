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
 * A `CliApp[R, E]` is a complete description of a command-line application, which requires environment `R`, and may
 * fail with a value of type `E`.
 */
sealed trait CliApp[-R, +E, +A] { self =>

  /**
   * Runs the CLI app, looking up dotfile defaults via the [[FileOptions]] service in the environment. This is the
   * primitive runner — `run` and `runWithoutFileArgs` below are convenience wrappers that pin a specific
   * [[FileOptions]] implementation.
   *
   * The current behaviour is:
   *   - if the underlying command has a single, deterministic top-level name (e.g. `git`, or `git push` whose root is
   *     `git`), [[FileOptions]] is asked for `.<name>` files in the cwd, all of its parents, and the user home
   *     directory, in that priority order;
   *   - if the root is an `OrElse` of distinct top-level commands (e.g. `(start | stop)`), there is no single name to
   *     anchor the lookup, so the file-options pass is silently skipped — addresses the design concern raised by the
   *     maintainer on the original PR (#317).
   *
   * Args originating from files are merged before user-supplied flags, with explicit command-line input always taking
   * precedence; closer files (cwd) override farther ones (parents, then home).
   */
  def runWithFileArgs(args: List[String]): ZIO[R & FileOptions, CliError[E], Option[A]]

  /**
   * Runs the CLI app with the platform-default [[FileOptions]] implementation: real file-system access on JVM and
   * Scala Native, no-op on Scala.js. Source-compatible with pre-#191 callers — the only behavioural change is that
   * `.<command>` files in the cwd / parents / home directory are now consulted on JVM/Native. Existing applications
   * that don't put any such files on disk see no difference at runtime.
   */
  final def run(args: List[String]): ZIO[R, CliError[E], Option[A]] =
    runWithFileArgs(args).provideSomeLayer[R](ZLayer.succeed(FileOptions.default))

  /**
   * Runs the CLI app with [[FileOptions.Noop]] explicitly wired in, suppressing all dotfile lookups. Useful in tests,
   * sandboxes, and any context where reading from the local filesystem is undesirable.
   */
  final def runWithoutFileArgs(args: List[String]): ZIO[R, CliError[E], Option[A]] =
    runWithFileArgs(args).provideSomeLayer[R](ZLayer.succeed[FileOptions](FileOptions.Noop))

  def config(newConfig: CliConfig): CliApp[R, E, A]

  final def map[B](f: A => B): CliApp[R, E, B] =
    self match {
      case CliApp.CliAppImpl(name, version, summary, command, execute, footer, config, figFont) =>
        CliApp.CliAppImpl(name, version, summary, command, execute.andThen(_.map(f)), footer, config, figFont)
    }

  def flatMap[R1 <: R, E1 >: E, B](f: A => ZIO[R1, E1, B]): CliApp[R1, E1, B]

  def footer(newFooter: HelpDoc): CliApp[R, E, A]

  def summary(s: HelpDoc.Span): CliApp[R, E, A]

}

object CliApp {

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

    override def runWithFileArgs(args: List[String]): ZIO[R & FileOptions, CliError[E], Option[A]] = {
      def executeBuiltIn(builtInOption: BuiltInOption): ZIO[R & FileOptions, CliError[E], Option[A]] =
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
              output <- runWithFileArgs(parameters)
            } yield output).catchSome { case CliError.BuiltIn(_) =>
              ZIO.none
            }
        }

      // prepend a first argument in case the CliApp's command is expected to consume it; also serves as the
      // unique-top-level-name probe consumed by file-options lookup below
      @tailrec
      def prefix(command: Command[_]): List[String] =
        command match {
          case Command.Single(name, _, _, _)  => List(name)
          case Command.Map(command, _)        => prefix(command)
          case Command.OrElse(_, _)           => Nil
          case Command.Subcommands(parent, _) => prefix(parent)
        }

      val rootName = prefix(self.command)

      // Only consult dotfiles when the root has a single deterministic name. For an `OrElse` root we'd otherwise be
      // arbitrarily picking one alias — see Kalin-Rudnicki's review of #317 for the original framing.
      val getFromFiles: ZIO[FileOptions, Nothing, List[FileOptions.OptionsFromFile]] =
        rootName.headOption match {
          case Some(name) => ZIO.serviceWithZIO[FileOptions](_.getOptionsFromFiles(name))
          case None =>
            ZIO.logDebug(
              "Skipping file-options lookup: top-level command has no unique name (`OrElse` root)."
            ) *> ZIO.succeed(Nil)
        }

      getFromFiles
        .flatMap { fromFiles =>
          self.command
            .parse(rootName ++ args, self.config, fromFiles)
        }
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
