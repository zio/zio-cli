package zio.cli

import zio.Console.printLine
import zio.System.envs
import zio._
import zio.cli.BuiltInOption._
import zio.cli.HelpDoc.Span.{code, text}
import zio.cli.HelpDoc.{h1, p}
import zio.cli.completion.{Completion, CompletionScript}
import zio.cli.figlet.FigFont
import java.nio.file.{Files, Paths}

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

    def checkAndGetOptionsFilePaths(topLevelCommand: String): List[String] = {
      val filename = s".$topLevelCommand"

      val cwd        = java.lang.System.getProperty("user.dir")
      val homeDirOpt = java.lang.System.getProperty("user.home")

      def parentPaths(path: String): List[String] = {
        val parts = path.split(java.io.File.separatorChar).filterNot(_.isEmpty)

        (0 to parts.length)
          .map(i =>
            s"${java.io.File.separatorChar}${parts
                .take(i)
                .mkString(java.io.File.separator)}"
          )
          .toList
      }

      val paths        = parentPaths(cwd)
      val pathsToCheck = homeDirOpt :: paths

      val existingFiles = pathsToCheck.filter { path =>
        val filePath = Paths.get(path, filename)
        Files.exists(filePath)
      }

      // Print out the paths with existing files
      existingFiles.foreach(println)

      return existingFiles
    }

    def loadOptionsFromFile(
      topLevelCommand: String
    ): List[String] = {
      // Get a list of file paths based on the top-level command
      val filePaths = checkAndGetOptionsFilePaths(topLevelCommand)

      // Read the contents of the files at the given file paths
      val options: List[String] = filePaths.flatMap { filePath =>
        val source = Source.fromFile(filePath)
        source.getLines.toList
      }

      /**
       * Merges a list of options, removing any duplicate keys.
       *
       * If there are options with the same keys but different values, it will use the value from the last option in the
       * list.
       *
       * @param options
       *   List of options in the format `--key=value`.
       * @return
       *   List of merged options.
       */
      def mergeOptionsBasedOnPriority(options: List[String]): List[String] = {
        // Create a map from the list, using option name as key and its value as value
        val mergedOptions = options.foldLeft(Map.empty[String, String]) { (acc, option) =>
          val Array(key, value) = option.split("=")
          acc + (key -> value)
        }

        // Convert the map back to a list
        mergedOptions.map { case (key, value) => s"$key=$value" }.toList
      }

      // Merge the read options
      val merged = mergeOptionsBasedOnPriority(options)

      // Return the merged options
      return merged
    }
    def run(args: List[String]): ZIO[R, Any, Any] = {
      def executeBuiltIn(builtInOption: BuiltInOption): ZIO[R, Any, Any] =
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
          case ShowWizard(command) => {
            val fancyName   = p(code(self.figFont.render(self.name)))
            val header      = p(text("WIZARD of ") + text(self.name) + text(self.version) + text(" -- ") + self.summary)
            val explanation = p(s"Wizard mode assist you in constructing commands for $name$version")

            (for {
              parameters <- Wizard(command, config, fancyName + header + explanation).execute
              _          <- run(parameters)
            } yield ()).catchSome { case Wizard.QuitException() =>
              ZIO.unit
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

      val arg_from_config_files = loadOptionsFromFile(self.name)
      self.command
        .parse(prefix(self.command) ++ arg_from_config_files ++ args, self.config)
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
