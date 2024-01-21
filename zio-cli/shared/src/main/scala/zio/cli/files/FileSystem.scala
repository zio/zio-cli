package zio.cli.files

import zio.{IO, UIO}
import zio.cli.PathPlatformSpecific

trait FileSystem extends PathPlatformSpecific {

  def parsePath(path: String): IO[String, JPath]

  def exists(path: JPath): UIO[Boolean]

  def isDirectory(path: JPath): UIO[Boolean]

  def isRegularFile(path: JPath): UIO[Boolean]

}

object FileSystem extends FileSystemPlatformSpecific {}
