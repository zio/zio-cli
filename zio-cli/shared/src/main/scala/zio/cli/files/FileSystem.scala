package zio.cli.files

import zio.{IO, UIO}
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

}

/** Provides `FileSystem.live`, the [[FileSystem]] of the platform. */
object FileSystem extends FileSystemPlatformSpecific {}
