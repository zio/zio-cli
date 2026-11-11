package zio.cli.files

import zio.{IO, UIO, ZIO}
import zio.cli.PathPlatformSpecific

trait FileSystem extends PathPlatformSpecific {

  def parsePath(path: String): IO[String, JPath]

  def exists(path: JPath): UIO[Boolean]

  def isDirectory(path: JPath): UIO[Boolean]

  def isRegularFile(path: JPath): UIO[Boolean]

  def getParent(path: JPath): UIO[Option[JPath]] = {
    val _ = path
    ZIO.none
  }

  def resolve(parent: JPath, child: String): IO[String, JPath] =
    parsePath(s"$parent/$child")

  def readFile(path: JPath): IO[String, String] =
    ZIO.fail(s"Cannot read file '$path'.")

  def currentWorkingDirectory: UIO[Option[String]] =
    zio.System.property("user.dir").orElseSucceed(None)

  def userHomeDirectory: UIO[Option[String]] =
    zio.System.property("user.home").orElseSucceed(None)

  def pathString(path: JPath): String =
    path.toString

}

object FileSystem extends FileSystemPlatformSpecific {}
