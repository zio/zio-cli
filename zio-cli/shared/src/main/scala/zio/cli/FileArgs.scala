package zio.cli

import zio._
import java.nio.file.Path

trait FileArgs {
  def getArgsFromFile(command: String): UIO[List[FileArgs.ArgsFromFile]]
}
object FileArgs extends FileArgsPlatformSpecific {

  final case class ArgsFromFile(path: String, args: List[String])

  case object Noop extends FileArgs {
    override def getArgsFromFile(command: String): UIO[List[ArgsFromFile]] = ZIO.succeed(Nil)
  }

  case object Live extends FileArgs {

    private def optReadPath(path: Path): UIO[Option[FileArgs.ArgsFromFile]] =
      (for {
        exists    <- ZIO.attempt(path.toFile.exists())
        pathString = path.toString
        optContents <-
          ZIO
            .readFile(pathString)
            .map(c => FileArgs.ArgsFromFile(pathString, c.split('\n').map(_.trim).filter(_.nonEmpty).toList))
            .when(exists)
      } yield optContents)
        .catchAllCause(ZIO.logErrorCause(s"Error reading options from file '$path', skipping...", _).as(None))

    private def getPathAndParents(path: Path): Task[List[Path]] =
      for {
        parentPath <- ZIO.attempt(Option(path.getParent))
        parents <- parentPath match {
                     case Some(parentPath) => getPathAndParents(parentPath)
                     case None             => ZIO.succeed(Nil)
                   }
      } yield path :: parents

    override def getArgsFromFile(command: String): UIO[List[ArgsFromFile]] =
      (for {
        cwd        <- System.property("user.dir")
        home       <- System.property("user.home")
        commandFile = s".$command"

        pathsFromCWD <- cwd match {
                          case Some(cwd) => ZIO.attempt(Path.of(cwd)).flatMap(getPathAndParents)
                          case None      => ZIO.succeed(Nil)
                        }
        homePath <- ZIO.foreach(home)(p => ZIO.attempt(Path.of(p)))
        allPaths  = (pathsFromCWD ::: homePath.toList).distinct

        argsFromFiles <- ZIO.foreach(allPaths) { path =>
                           ZIO.attempt(path.resolve(commandFile)).flatMap(optReadPath)
                         }
      } yield argsFromFiles.flatten)
        .catchAllCause(ZIO.logErrorCause(s"Error reading options from files, skipping...", _).as(Nil))

  }

}
