package zio.cli

import zio._

import java.nio.file.Path

/**
 * `FileOptions` is the service [[CliApp]] uses to look up command-line option defaults from "dotfiles" on disk.
 *
 * The contract is intentionally narrow: given the *unique* top-level command name (e.g. `git`), return the contents of
 * any matching `.<name>` files found in a well-defined search path (current working directory, all of its parents, then
 * the user home directory). Each line of those files is treated as a single CLI argument.
 *
 * Files closer to the working directory take precedence over more distant ones, and explicit command-line arguments
 * always take precedence over file-derived ones. Files that do not exist are silently skipped; files that exist but are
 * unreadable produce a debug log entry but do not abort parsing.
 *
 * The service abstraction allows callers to substitute [[FileOptions.Noop]] (e.g. in tests, in a sandbox, or on JS
 * where there is no file system) without touching the `CliApp` itself.
 */
trait FileOptions {

  /**
   * Returns the dotfile contents associated with the given top-level command name, in priority order from highest (most
   * specific / closest to the cwd) to lowest. Each [[FileOptions.OptionsFromFile]] carries the path it came from so
   * downstream merging logic can report provenance.
   */
  def getOptionsFromFiles(command: String): UIO[List[FileOptions.OptionsFromFile]]
}

object FileOptions extends FileOptionsPlatformSpecific {

  /**
   * A single dotfile worth of arguments. `path` is purely informational — used only for logging which file an
   * overridden option originated from.
   */
  final case class OptionsFromFile(path: String, rawArgs: List[String])

  /**
   * No-op implementation. Useful for tests and for the JS platform, which has no file-system access.
   */
  case object Noop extends FileOptions {
    override def getOptionsFromFiles(command: String): UIO[List[OptionsFromFile]] = ZIO.succeed(Nil)
  }

  /**
   * Default JVM/Native implementation that walks the file system to discover `.<command>` files.
   *
   * Search order, highest priority first:
   *   1. `<cwd>/.<command>`
   *   2. `<each parent of cwd, walking upward>/.<command>`
   *   3. `<user.home>/.<command>`
   *
   * This ordering means project-local overrides win over user-global defaults — the convention most CLI tools follow.
   */
  case object Live extends FileOptions {

    private def optReadPath(path: Path): UIO[Option[FileOptions.OptionsFromFile]] =
      (for {
        _           <- ZIO.logDebug(s"Searching for file options in '$path'")
        exists      <- ZIO.attempt(path.toFile.exists())
        pathString   = path.toString
        optContents <-
          ZIO
            .readFile(pathString)
            .map(c => FileOptions.OptionsFromFile(pathString, c.split('\n').map(_.trim).filter(_.nonEmpty).toList))
            .when(exists)
      } yield optContents)
        .catchAllCause(ZIO.logErrorCause(s"Error reading options from file '$path', skipping...", _).as(None))

    private def getPathAndParents(path: Path): Task[List[Path]] =
      for {
        parentPath <- ZIO.attempt(Option(path.getParent))
        parents    <- parentPath match {
                     case Some(parentPath) => getPathAndParents(parentPath)
                     case None             => ZIO.succeed(Nil)
                   }
      } yield path :: parents

    override def getOptionsFromFiles(command: String): UIO[List[OptionsFromFile]] =
      (for {
        cwd        <- System.property("user.dir")
        home       <- System.property("user.home")
        commandFile = s".$command"

        pathsFromCWD <- cwd match {
                          case Some(cwd) => ZIO.attempt(Path.of(cwd)).flatMap(getPathAndParents)
                          case None      => ZIO.succeed(Nil)
                        }
        homePath <- ZIO.foreach(home)(p => ZIO.attempt(Path.of(p)))
        allPaths  = (pathsFromCWD ::: homePath.toList).distinct

        argsFromFiles <- ZIO.foreach(allPaths) { path =>
                           ZIO.attempt(path.resolve(commandFile)).flatMap(optReadPath)
                         }
      } yield argsFromFiles.flatten)
        .catchAllCause(ZIO.logErrorCause("Error reading options from files, skipping...", _).as(Nil))

  }

}
