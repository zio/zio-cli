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
import java.io.IOException
import java.nio.file.Files
import java.nio.file.Path
import scala.io.Source
import java.nio.file.Paths

/**
 * A `CliApp[R, E]` is a complete description of a command-line application, which requires environment `R`, and may
 * fail with a value of type `E`.
 */
sealed trait CliApp[-R, +E, +A] { self =>

  def run(args: List[String]): ZIO[R, CliError[E], Option[A]]

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

  def findPathsOfCliConfigFiles(topLevelCommand: String): Task[List[String]] = {
    val filename   = s".$topLevelCommand"
    val cwd        = java.lang.System.getProperty("user.dir")
    val homeDirOpt = java.lang.System.getProperty("user.home")

    def parentPaths(path: String): List[String] = {
      val parts = path.split(java.io.File.separatorChar).filterNot(_.isEmpty)
      (0 to parts.length)
        .map(i => s"${java.io.File.separatorChar}${parts.take(i).mkString(java.io.File.separator)}")
        .toList
    }

    val paths        = parentPaths(cwd)
    val pathsToCheck = homeDirOpt :: paths

    // Use ZIO to filter the paths
    ZIO
      .foreach(pathsToCheck)(path => ZIO.succeed(Files.exists(Path.of(path, filename))))
      .map(_.zip(pathsToCheck).collect { case (exists, path) if exists => path })
      .map(_.distinct) // Remove them duplicates

  }

  //  Merges a list of options, removing any duplicate keys.
  //  If there are options with the same keys but different values, it will use the value from the last option in the
  //  list.
  def mergeOptionsBasedOnPriority(options: List[String]): List[String] = {
    val mergedOptions = options.flatMap { opt =>
      opt.split('=') match {
        case Array(key)        => Some(key -> None)
        case Array(key, value) => Some(key -> value)
        case _ =>
          None // handles the case when there isn't exactly one '=' in the string
      }
    }.toMap.toList.map {
      case (key, None)  => key
      case (key, value) => s"$key=$value"
    }

    mergedOptions
  }

  def loadOptionsFromConfigFiles(topLevelCommand: String): ZIO[Any, IOException, List[String]] =
    for {
      filePaths <- findPathsOfCliConfigFiles(topLevelCommand)
                     .refineToOrDie[IOException]
      lines <- ZIO
                 .foreach(filePaths) { filePath =>
                   ZIO.acquireReleaseWith(
                     ZIO.attempt(
                       Source.fromFile(Paths.get(filePath, "." + topLevelCommand).toFile)
                     )
                   )(source => ZIO.attempt(source.close()).orDie) { source =>
                     ZIO.attempt(source.getLines().toList.filter(_.trim.nonEmpty))
                   }
                 }
                 .map(_.flatten)
                 .refineToOrDie[IOException]
    } yield lines

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

    def printDocs(helpDoc: HelpDoc): UIO[Unit] =
      printLine(helpDoc.toPlaintext(80)).!

    def run(args: List[String]): ZIO[R, CliError[E], Option[A]] = {
      def executeBuiltIn(builtInOption: BuiltInOption): ZIO[R, CliError[E], Option[A]] =
        builtInOption match {
          case ShowHelp(synopsis, helpDoc) =>
            val fancyName = p(code(self.figFont.render(self.name)))

            val header = p(text(self.name) + text(self.version) + text(" -- ") + self.summary)

            val synopsisHelpDoc = h1("usage") + synopsis
              .enumerate(config)
              .map(span => text("$ ") + span)
              .map(HelpDoc.p)
              .foldRight(HelpDoc.empty)(_ + _)

            // TODO add rendering of built-in options such as help
            printLine(
              (fancyName + header + synopsisHelpDoc + helpDoc + self.footer).toPlaintext(columnWidth = 300)
            ).map(_ => None).mapError(CliError.IO(_))

          case ShowCompletionScript(path, shellType) =>
            printLine(
              CompletionScript(path, if (self.command.names.nonEmpty) self.command.names else Set(self.name), shellType)
            ).map(_ => None).mapError(CliError.IO(_))
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
            }.map(_ => None).mapError(CliError.BuiltIn(_))
          case ShowWizard(command) => {
            val fancyName   = p(code(self.figFont.render(self.name)))
            val header      = p(text("WIZARD of ") + text(self.name) + text(self.version) + text(" -- ") + self.summary)
            val explanation = p(s"Wizard mode assist you in constructing commands for $name$version")

            (for {
              parameters <-
                Wizard(command, config, fancyName + header + explanation).execute.mapError(CliError.BuiltIn(_))
              output <- run(parameters)
            } yield output).catchSome { case CliError.BuiltIn(_) =>
              ZIO.succeed(None)
            }
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

      // Reading args from config files and combining with provided args
      val combinedArgs: ZIO[R, CliError[E], List[String]] =
        loadOptionsFromConfigFiles(self.command.names.head).flatMap { configArgs =>
          ZIO.succeed(mergeOptionsBasedOnPriority(configArgs ++ args))
        }.mapError(e => CliError.IO(e))

      combinedArgs.flatMap { allArgs =>
        self.command
          .parse(prefix(self.command) ++ allArgs, self.config)
          .foldZIO(
            e => printDocs(e.error) *> ZIO.fail(CliError.Parsing(e)),
            {
              case CommandDirective.UserDefined(_, value) =>
                self.execute(value).map(Some(_)).mapError(CliError.Execution(_))
              case CommandDirective.BuiltIn(x) =>
                executeBuiltIn(x).catchSome { case err @ CliError.Parsing(e) =>
                  printDocs(e.error) *> ZIO.fail(err)
                }
            }
          )
      }
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
