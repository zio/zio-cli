package zio.cli.files

import zio.{IO, UIO, ZIO}
import zio.cli.PathPlatformSpecific

import java.nio.file.{Files => JFiles, Paths => JPaths}

private[cli] trait FileSystemPlatformSpecific extends PathPlatformSpecific {

  /**
   * The [[FileSystem]] of the platform. On the JVM and Scala Native it uses `java.nio.file.Paths.get` and
   * `java.nio.file.Files`: `parsePath` fails with `'<path>' is not a recognized path.` when `Paths.get` rejects the
   * string (on the JVM, a string with a NUL character), and a path that does not exist still parses. `exists`,
   * `isDirectory` and `isRegularFile` return `false` for a missing path, and also when the check throws. On Scala.js,
   * `parsePath` fails with `FileSystem is not available in ScalaJS`, and `exists`, `isDirectory` and `isRegularFile`
   * return `false`.
   */
  val live: FileSystem = new FileSystem {

    override def parsePath(path: String): IO[String, JPath] =
      ZIO.attempt(JPaths.get(path)) orElseFail s"'$path' is not a recognized path."

    override def exists(path: JPath): UIO[Boolean] =
      ZIO.attempt(JFiles.exists(path)) orElse ZIO.succeed(false)

    override def isDirectory(path: JPath): UIO[Boolean] =
      ZIO.attempt(JFiles.isDirectory(path)) orElse ZIO.succeed(false)

    override def isRegularFile(path: JPath): UIO[Boolean] =
      ZIO.attempt(JFiles.isRegularFile(path)) orElse ZIO.succeed(false)
  }

}
