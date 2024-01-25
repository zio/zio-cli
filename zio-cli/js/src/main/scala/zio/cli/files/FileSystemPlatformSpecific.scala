package zio.cli.files

import zio.{IO, UIO, ZIO}
import zio.cli.PathPlatformSpecific

private[cli] trait FileSystemPlatformSpecific extends PathPlatformSpecific {

  val live: FileSystem = new FileSystem {

    override def parsePath(path: String): IO[String, JPath] =
      ZIO.fail("FileSystem is not available in ScalaJS")

    override def exists(path: JPath): UIO[Boolean] =
      ZIO.succeed(false)

    override def isDirectory(path: JPath): UIO[Boolean] =
      ZIO.succeed(false)

    override def isRegularFile(path: JPath): UIO[Boolean] =
      ZIO.succeed(false)
  }

}
