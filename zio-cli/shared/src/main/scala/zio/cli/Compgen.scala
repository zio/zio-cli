package zio.cli.completion

import zio._

/**
 * Lists the file and directory names that start with a word, for the shell completion of path arguments. On the JVM, `Compgen.live` and `Compgen.test` run Bash's built-in `compgen` command; on Scala.js and Scala Native they return empty lists.
 */
trait Compgen {

  /** The error type of the completion effects. */
  type CommandError <: Throwable

  /**
   * Returns the files and directories whose names start with `word`: first the files, each followed by a space, then the directories, each followed by `/`. A name that matches nothing gives an empty list.
   */
  def completeFileNames(word: String): ZIO[Any, CommandError, List[String]]

  /**
   * Returns the directories whose names start with `word`, each followed by `/`. A name that matches nothing gives an empty list.
   */
  def completeDirectoryNames(word: String): ZIO[Any, CommandError, List[String]]
}

/**
 * Provides `Compgen.live`, which completes names in the working directory of the process, and `Compgen.test`, which completes names in a given directory.
 */
object Compgen extends CompgenPlatformSpecific {}
