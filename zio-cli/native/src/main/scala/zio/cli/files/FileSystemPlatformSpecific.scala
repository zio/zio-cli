package zio.cli.files

import zio.{IO, UIO, ZIO}
import zio.cli.PathPlatformSpecific

import java.nio.file.{Files => JFiles, Paths => JPaths}

private[cli] trait FileSystemPlatformSpecific extends PathPlatformSpecific {

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