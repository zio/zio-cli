package zio.cli.completion

import zio._
import java.io.File
import scala.annotation.nowarn

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

  @nowarn
  private def create(workingDirectory: Option[File]): Compgen = new Compgen {

    type CommandError = Nothing

    def completeFileNames(word: String): ZIO[Any, CommandError, List[String]] = ZIO.succeed(Nil)

    def completeDirectoryNames(word: String): ZIO[Any, CommandError, List[String]] = ZIO.succeed(Nil)

  }
}
