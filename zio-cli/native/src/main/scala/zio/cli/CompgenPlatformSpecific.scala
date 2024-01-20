package zio.cli.completion

import zio._
import java.io.File
import scala.annotation.nowarn

/**
 * `Compgen` simplifies the process of calling Bash's built-in `compgen` command.
 */

trait CompgenPlatformSpecific {
  def live: Compgen                         = create(None)
  def test(workingDirectory: File): Compgen = create(Some(workingDirectory))

  @nowarn
  private def create(workingDirectory: Option[File]): Compgen = new Compgen {

    type CommandError = Nothing
    
    def completeFileNames(word: String): ZIO[Any, CommandError, List[String]] = ZIO.succeed(Nil)

    def completeDirectoryNames(word: String): ZIO[Any, CommandError, List[String]] = ZIO.succeed(Nil)

  }
}
