package zio.cli.completion

import zio._
import zio.process._
import java.io.File

/**
 * Builds the [[Compgen]] instances of the platform. On the JVM they run Bash's built-in `compgen` command, and their `CommandError` is `zio.process.CommandError`; on Scala.js and Scala Native they return empty lists, and their `CommandError` is `Nothing`.
 */
trait CompgenPlatformSpecific {
  /**
   * A [[Compgen]] that completes names relative to the working directory of the process. It returns empty lists on Scala.js and Scala Native.
   */
  def live: Compgen                         = create(None)

  /**
   * A [[Compgen]] that completes names relative to `workingDirectory`. It returns empty lists on Scala.js and Scala Native.
   */
  def test(workingDirectory: File): Compgen = create(Some(workingDirectory))

  private def create(workingDirectory: Option[File]): Compgen = new Compgen {

    type CommandError = zio.process.CommandError

    def completeFileNames(word: String): ZIO[Any, CommandError, List[String]] =
      /*
       * For file names, we want the cursor to skip forward to the next argument
       * position, so we append a space (" ") to the end of them below. For
       * directory names, however, we don't want to skip to the next argument
       * position, because we like being able to smash the tab key to keep walking
       * down through a directory tree.
       */
      for {
        files        <- runBashCommand(s"compgen -f -- $word")
        directories  <- completeDirectoryNames(word)
        directorySet  = directories.toSet
        filesFiltered = files.filter(file => !directorySet(file + "/"))
      } yield filesFiltered.map(_ + " ") ++ directories

    def completeDirectoryNames(word: String): ZIO[Any, CommandError, List[String]] =
      runBashCommand(s"compgen -o nospace -d -S / -- $word")

    private def runBashCommand(command: String): ZIO[Any, CommandError, List[String]] =
      Command
        .Standard(
          NonEmptyChunk("bash", "-c", command),
          Map.empty,
          workingDirectory,
          ProcessInput.Inherit,
          ProcessOutput.Pipe,
          ProcessOutput.Pipe,
          redirectErrorStream = false
        )
        .lines
        .map(_.toList)
  }
}
