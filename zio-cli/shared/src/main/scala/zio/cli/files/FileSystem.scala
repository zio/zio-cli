package zio.cli.files

import zio.{IO, UIO}
import java.nio.file.{Files => JFiles, Path => JPath, Paths => JPaths}

trait FileSystem {

  def parsePath(path: String): IO[String, JPath]

  def exists(path: JPath): UIO[Boolean]

  def isDirectory(path: JPath): UIO[Boolean]

  def isRegularFile(path: JPath): UIO[Boolean]

}

object FileSystem {

  def live: FileSystem = new FileSystem {
    override def parsePath(path: String) = IO.effect(JPaths.get(path)) orElseFail (s"'$path' is not a recognized path.")

    override def exists(path: JPath) = IO.effect(JFiles.exists(path)) orElse IO.succeed(false)

    override def isDirectory(path: JPath) = IO.effect(JFiles.isDirectory(path)) orElse IO.succeed(false)

    override def isRegularFile(path: JPath) = IO.effect(JFiles.isRegularFile(path)) orElse IO.succeed(false)
  }

}
