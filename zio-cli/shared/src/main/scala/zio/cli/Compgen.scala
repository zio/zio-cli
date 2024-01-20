package zio.cli.completion

import zio._

/**
 * `Compgen` simplifies the process of calling Bash's built-in `compgen` command.
 */
trait Compgen {
  
  type CommandError <: Throwable

  def completeFileNames(word: String): ZIO[Any, CommandError, List[String]]

  def completeDirectoryNames(word: String): ZIO[Any, CommandError, List[String]]
}

object Compgen extends CompgenPlatformSpecific {

}
