package zio.cli

import zio.Console.printLine
import zio.System.envs
import zio._
import zio.cli.BuiltInOption._
import zio.cli.HelpDoc.Span.{code, text}
import zio.cli.HelpDoc.{h1, p}
import zio.cli.completion.{Completion, CompletionScript}
import zio.cli.figlet.FigFont
import zio.cli.files.FileSystem

import scala.annotation.tailrec

/**
 * Loads default command-line options from `.<command>` configuration files on the file system.
 */
trait FileOptions {

  /**
   * Returns the `.<command>` option files found for `command`, ordered from highest priority (current working
   * directory) to lowest priority (user home directory).
   */
  def getOptionsFromFiles(command: String): UIO[List[FileOptions.OptionsFromFile]]
}

/**
 * Implementations of [[FileOptions]].
 */
object FileOptions {

  /**
   * Raw option tokens read from a single configuration file at `path`.
   */
  final case class OptionsFromFile(path: String, rawArgs: List[String])

  /**
   * A [[FileOptions]] implementation that ignores configuration files and returns no options.
   */
  case object Noop extends FileOptions {
    override def getOptionsFromFiles(command: String): UIO[List[OptionsFromFile]] = {
      val _ = command
      ZIO.succeed(Nil)
    }
  }

  /**
   * Reads `.<command>` files from the current working directory, all of its parent directories, and the user home
   * directory (`~`) using `fileSystem`, deduplicating the user home directory so it always retains lowest priority.
   */
  final case class FromFileSystem(fileSystem: FileSystem) extends FileOptions {

    private def stripQuotes(s: String): String =
      if (s.length >= 2 && ((s.startsWith("\"") && s.endsWith("\"")) || (s.startsWith("'") && s.endsWith("'"))))
        s.substring(1, s.length - 1)
      else s

    private def tokenize(contents: String): List[String] =
      contents.linesIterator
        .map(_.trim)
        .filter(line => line.nonEmpty && !line.startsWith("#"))
        .flatMap { line =>
          val firstEq  = line.indexOf('=')
          val firstWs  = line.indexWhere(_.isWhitespace)
          val splitIdx =
            if (firstEq > 0 && (firstWs < 0 || firstEq < firstWs)) firstEq
            else if (firstWs > 0) firstWs
            else -1
          if (line.startsWith("-") && splitIdx > 0) {
            val flag  = line.substring(0, splitIdx)
            val value = stripQuotes(line.substring(splitIdx + 1).trim)
            List(flag, value)
          } else {
            List(stripQuotes(line))
          }
        }
        .toList

    private def optReadPath(path: fileSystem.JPath): UIO[Option[OptionsFromFile]] = {
      val pathStr = fileSystem.pathString(path)
      (for {
        _           <- ZIO.logDebug(s"Searching for file options in '$pathStr'")
        exists      <- fileSystem.isRegularFile(path)
        optContents <-
          fileSystem
            .readFile(path)
            .map(c => OptionsFromFile(pathStr, tokenize(c)))
            .when(exists)
      } yield optContents)
        .catchAllCause(ZIO.logErrorCause(s"Error reading options from file '$pathStr', skipping...", _).as(None))
    }

    private def getPathAndParents(path: fileSystem.JPath): UIO[List[fileSystem.JPath]] = {
      def loop(
        current: fileSystem.JPath,
        visited: Set[String],
        acc: List[fileSystem.JPath]
      ): UIO[List[fileSystem.JPath]] = {
        val currentStr = fileSystem.pathString(current)
        if (visited.contains(currentStr)) ZIO.succeed(acc.reverse)
        else
          fileSystem.getParent(current).flatMap {
            case Some(parent) => loop(parent, visited + currentStr, current :: acc)
            case None         => ZIO.succeed((current :: acc).reverse)
          }
      }
      loop(path, Set.empty, Nil)
    }

    override def getOptionsFromFiles(command: String): UIO[List[OptionsFromFile]] =
      (for {
        cwd          <- fileSystem.currentWorkingDirectory
        home         <- fileSystem.userHomeDirectory
        commandFile   = s".$command"
        pathsFromCwd <- cwd match {
                          case Some(c) if c.nonEmpty =>
                            fileSystem.parsePath(c).foldZIO(_ => ZIO.succeed(Nil), getPathAndParents)
                          case _ => ZIO.succeed(Nil)
                        }
        homePathOpt <- home match {
                         case Some(h) if h.nonEmpty => fileSystem.parsePath(h).option
                         case _                     => ZIO.none
                       }
        allPaths = {
          val homeKeyOpt  = homePathOpt.map(fileSystem.pathString)
          val cwdFiltered = pathsFromCwd.filterNot(p => homeKeyOpt.contains(fileSystem.pathString(p)))
          val combined    = cwdFiltered ++ homePathOpt.toList
          combined
            .foldLeft((List.empty[fileSystem.JPath], Set.empty[String])) { case ((acc, seen), p) =>
              val key = fileSystem.pathString(p)
              if (seen.contains(key)) (acc, seen)
              else (acc :+ p, seen + key)
            }
            ._1
        }
        optionsFromFiles <- ZIO.foreach(allPaths) { dirPath =>
                              fileSystem.resolve(dirPath, commandFile).foldZIO(_ => ZIO.none, optReadPath)
                            }
      } yield optionsFromFiles.flatten)
        .catchAllCause(ZIO.logErrorCause("Error reading options from files, skipping...", _).as(Nil))
  }

  /**
   * The live [[FileOptions]] backed by `FileSystem.live`.
   */
  val Live: FileOptions = FromFileSystem(FileSystem.live)

  /**
   * The default [[FileOptions]] used by [[CliApp.run]].
   */
  val default: FileOptions = Live
}

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
   * Parses `args` together with any `.<name>` option files supplied by the [[FileOptions]] service in the environment,
   * and runs the application.
   */
  def runWithFileArgs(args: List[String]): ZIO[R with FileOptions, CliError[E], Option[A]]

  /**
   * Parses `args`, the words typed after the program's name, merges any options found in `.<name>` configuration files
   * via `FileOptions.default`, and runs the application. For a single command, or a command with subcommands, the
   * command's own name is added in front of `args`, so users don't type it; for alternatives built with `|`, the first
   * word picks the command.
   *
   * Returns `Some(result)` after running the application's function, or `None` after a built-in option such as
   * `--help`. Words left over after the command's arguments are ignored. On a parse error, it prints the error to the
   * console and fails with `CliError.Parsing`; a failure of the application's function becomes `CliError.Execution`.
   */
  final def run(args: List[String]): ZIO[R, CliError[E], Option[A]] =
    runWithFileArgs(args).provideSomeLayer[R](ZLayer.succeed[FileOptions](FileOptions.default))

  /**
   * Runs the application with `args` only, ignoring `.<name>` configuration files (`FileOptions.Noop`).
   */
  final def runWithoutFileArgs(args: List[String]): ZIO[R, CliError[E], Option[A]] =
    runWithFileArgs(args).provideSomeLayer[R](ZLayer.succeed[FileOptions](FileOptions.Noop))

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

    override def runWithFileArgs(args: List[String]): ZIO[R with FileOptions, CliError[E], Option[A]] = {
      // prepend a first argument in case the CliApp's command is expected to consume it
      @tailrec
      def prefix(command: Command[_]): List[String] =
        command match {
          case Command.Single(cmdName, _, _, _) => List(cmdName)
          case Command.Map(command, _)          => prefix(command)
          case Command.OrElse(_, _)             => Nil
          case Command.Subcommands(parent, _)   => prefix(parent)
        }

      val rootPrefix = prefix(self.command)
      val allArgs    = rootPrefix ++ args

      def executeBuiltIn(builtInOption: BuiltInOption): ZIO[R with FileOptions, CliError[E], Option[A]] =
        builtInOption match {
          case ShowHelp(synopsis, helpDoc) =>
            val fancyName = p(code(self.figFont.render(self.name)))

            val header = p(text(self.name) + text(" ") + text(self.version) + text(" -- ") + self.summary)

            val synopsisHelpDoc = h1("usage") + synopsis
              .enumerate(config)
              .map(span => text("$ ") + span)
              .map(HelpDoc.p)
              .foldRight(HelpDoc.empty)(_ + _)

            val fileOptionsHelpDoc = {
              val dotfileNames =
                rootPrefix.headOption.map(n => s".$n").toList match {
                  case Nil  => self.command.names.toList.sorted.map(n => s".$n")
                  case list => list
                }
              if (dotfileNames.isEmpty) HelpDoc.empty
              else {
                val filesLabel = dotfileNames.map(n => s"'$n'").mkString(", ")
                h1("file options") +
                  p(
                    s"Options can be read from $filesLabel configuration file(s) in the current working directory, all parent directories of the current working directory, and the user home directory (~), with one option per line (e.g. --flag=value or --flag value; lines starting with '#' are ignored)."
                  ) +
                  p(
                    "Priority order (highest to lowest): command-line arguments > current working directory > parent directories > user home directory (~)."
                  )
              }
            }

            // TODO add rendering of built-in options such as help
            printLine(
              (fancyName + header + synopsisHelpDoc + helpDoc + fileOptionsHelpDoc + self.footer)
                .toPlaintext(columnWidth = 300)
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

      val resolvedCommandName: Option[String] =
        rootPrefix.headOption.orElse {
          allArgs.headOption.flatMap { firstArg =>
            self.command.names.find(n => self.config.normalizeCase(n) == self.config.normalizeCase(firstArg))
          }
        }

      val getFromFiles: ZIO[FileOptions, Nothing, List[FileOptions.OptionsFromFile]] =
        resolvedCommandName match {
          case Some(cmdName) => ZIO.serviceWithZIO[FileOptions](_.getOptionsFromFiles(cmdName))
          case None          =>
            ZIO.logDebug(
              "Skipping file-options lookup: top-level command has no unique name."
            ) *> ZIO.succeed(Nil)
        }

      getFromFiles.flatMap { fromFiles =>
        self.command
          .parse(allArgs, self.config, fromFiles)
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
