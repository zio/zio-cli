package zio.cli.files

import zio.{IO, UIO, ZIO}

import java.nio.file.{Files => JFiles, Path => JPath, Paths => JPaths}

trait FileSystem {

  def parsePath(path: String): IO[String, JPath]

  def exists(path: JPath): UIO[Boolean]

  def isDirectory(path: JPath): UIO[Boolean]

  def isRegularFile(path: JPath): UIO[Boolean]

}

object FileSystem {

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
