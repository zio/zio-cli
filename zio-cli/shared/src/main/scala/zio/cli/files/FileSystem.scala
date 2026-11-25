package zio.cli.files

import zio.{IO, UIO, ZIO}
import zio.cli.PathPlatformSpecific

/**
 * The file-system operations that `PrimType.Path` uses to parse a path and to check that it exists and is a file or a
 * directory. `FileSystem.live` uses the file system of the platform; pass another implementation to `PrimType.Path` to
 * check paths without the real file system.
 */
trait FileSystem extends PathPlatformSpecific {

  /**
   * Parses `path` into a path of the platform (`java.nio.file.Path` on the JVM and Scala Native, `String` on Scala.js),
   * or fails with an error message. It does not check that the path exists.
   */
  def parsePath(path: String): IO[String, JPath]

  /** Returns whether a file or directory exists at `path`. */
  def exists(path: JPath): UIO[Boolean]

  /** Returns whether `path` is an existing directory. */
  def isDirectory(path: JPath): UIO[Boolean]

  /** Returns whether `path` is an existing regular file. */
  def isRegularFile(path: JPath): UIO[Boolean]

  /** Returns the parent directory of `path`, or `None` when `path` has no parent. */
  def getParent(path: JPath): UIO[Option[JPath]] = {
    val _ = path
    ZIO.none
  }

  /** Resolves `child` against `parent`, or fails with an error message. */
  def resolve(parent: JPath, child: String): IO[String, JPath] =
    parsePath(s"$parent/$child")

  /** Reads the UTF-8 contents of the file at `path`, or fails with an error message. */
  def readFile(path: JPath): IO[String, String] =
    ZIO.fail(s"Cannot read file '$path'.")

  /** Returns the current working directory (`user.dir`, or `process.cwd()` on Scala.js under Node.js), if available. */
  def currentWorkingDirectory: UIO[Option[String]] =
    zio.System.property("user.dir").orElseSucceed(None)

  /** Returns the user's home directory (`user.home`, or `os.homedir()` on Scala.js under Node.js), if available. */
  def userHomeDirectory: UIO[Option[String]] =
    zio.System.property("user.home").orElseSucceed(None)

  /** Returns a string representation of `path` (normalized by platform implementations such as `FileSystem.live`). */
  def pathString(path: JPath): String =
    path.toString

}

/** Provides `FileSystem.live`, the [[FileSystem]] of the platform. */
object FileSystem extends FileSystemPlatformSpecific {}
