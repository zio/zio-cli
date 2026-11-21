package zio.cli.files

import zio.{IO, UIO, ZIO}
import zio.cli.PathPlatformSpecific

import scala.scalajs.js

private[cli] trait FileSystemPlatformSpecific extends PathPlatformSpecific {

  val live: FileSystem = new FileSystem {
    private lazy val nodeFs: Option[js.Dynamic] =
      scala.util
        .Try(js.Dynamic.global.require("fs"))
        .toOption
        .filter(m => !js.isUndefined(m) && m != null)

    private lazy val nodePath: Option[js.Dynamic] =
      scala.util
        .Try(js.Dynamic.global.require("path"))
        .toOption
        .filter(m => !js.isUndefined(m) && m != null)

    private lazy val nodeOs: Option[js.Dynamic] =
      scala.util
        .Try(js.Dynamic.global.require("os"))
        .toOption
        .filter(m => !js.isUndefined(m) && m != null)

    override def currentWorkingDirectory: UIO[Option[String]] =
      zio.System.property("user.dir").orElseSucceed(None).flatMap {
        case Some(dir) if dir.nonEmpty && dir != "/" => ZIO.some(dir)
        case _                                       =>
          ZIO
            .attempt(js.Dynamic.global.process.cwd().asInstanceOf[String])
            .map(s => if (s != null && s.nonEmpty) Some(s) else None)
            .orElseSucceed(None)
      }

    override def userHomeDirectory: UIO[Option[String]] =
      zio.System.property("user.home").orElseSucceed(None).flatMap {
        case Some(home) if home.nonEmpty && home != "/" => ZIO.some(home)
        case _                                          =>
          nodeOs match {
            case Some(os) =>
              ZIO
                .attempt(os.homedir().asInstanceOf[String])
                .map(s => if (s != null && s.nonEmpty) Some(s) else None)
                .orElseSucceed(None)
            case None => ZIO.none
          }
      }

    override def parsePath(path: String): IO[String, JPath] =
      if (path.isEmpty) ZIO.fail("'' is not a recognized path.")
      else
        nodePath match {
          case Some(p) =>
            ZIO.attempt(p.resolve(path).asInstanceOf[String]).orElseFail(s"'$path' is not a recognized path.")
          case None =>
            ZIO.fail("FileSystem is not available in ScalaJS")
        }

    override def exists(path: JPath): UIO[Boolean] =
      nodeFs match {
        case Some(fs) => ZIO.attempt(fs.existsSync(path).asInstanceOf[Boolean]).orElseSucceed(false)
        case None     => ZIO.succeed(false)
      }

    override def isDirectory(path: JPath): UIO[Boolean] =
      nodeFs match {
        case Some(fs) => ZIO.attempt(fs.statSync(path).isDirectory().asInstanceOf[Boolean]).orElseSucceed(false)
        case None     => ZIO.succeed(false)
      }

    override def isRegularFile(path: JPath): UIO[Boolean] =
      nodeFs match {
        case Some(fs) => ZIO.attempt(fs.statSync(path).isFile().asInstanceOf[Boolean]).orElseSucceed(false)
        case None     => ZIO.succeed(false)
      }

    override def getParent(path: JPath): UIO[Option[JPath]] =
      nodePath match {
        case Some(p) =>
          ZIO.attempt {
            val parent = p.dirname(path).asInstanceOf[String]
            if (parent.isEmpty || parent == path) None else Some(parent)
          }.orElseSucceed(None)
        case None => ZIO.none
      }

    override def resolve(parent: JPath, child: String): IO[String, JPath] =
      nodePath match {
        case Some(p) =>
          ZIO.attempt(p.join(parent, child).asInstanceOf[String]).orElseFail(s"Cannot resolve '$child' in '$parent'.")
        case None =>
          ZIO.fail("FileSystem is not available in ScalaJS")
      }

    override def readFile(path: JPath): IO[String, String] =
      nodeFs match {
        case Some(fs) =>
          ZIO.attempt(fs.readFileSync(path, "utf8").asInstanceOf[String]).orElseFail(s"Cannot read file '$path'.")
        case None =>
          ZIO.fail("FileSystem is not available in ScalaJS")
      }
  }

}
