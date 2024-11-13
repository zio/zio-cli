package zio.cli

import zio._
import java.nio.file.Path

trait FileOptions {
  def getOptionsFromFiles(command: String): UIO[List[FileOptions.OptionsFromFile]]
}
object FileOptions extends FileOptionsPlatformSpecific {

  final case class OptionsFromFile(path: String, rawArgs: List[String])

  case object Noop extends FileOptions {
    override def getOptionsFromFiles(command: String): UIO[List[OptionsFromFile]] = ZIO.succeed(Nil)
  }

  case object Live extends FileOptions {

    private def optReadPath(path: Path): UIO[Option[FileOptions.OptionsFromFile]] =
      (for {
        _         <- ZIO.logDebug(s"Searching for file options in '$path'")
        exists    <- ZIO.attempt(path.toFile.exists())
        pathString = path.toString
        optContents <-
          ZIO
            .readFile(pathString)
            .map(c => FileOptions.OptionsFromFile(pathString, c.split('\n').map(_.trim).filter(_.nonEmpty).toList))
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

    override def getOptionsFromFiles(command: String): UIO[List[OptionsFromFile]] =
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
