package zio.cli

import java.io.IOException
import zio._
import java.nio.file.{Files, Path, Paths}
import scala.io.Source

object ConfigFileArgsPlatformSpecific extends ConfigFilePlatformSpecific {
  def findPathsOfCliConfigFiles(topLevelCommand: String): Task[List[String]] = {
    val filename   = s".$topLevelCommand"
    val cwd        = java.lang.System.getProperty("user.dir")
    val homeDirOpt = java.lang.System.getProperty("user.home")

    def parentPaths(path: String): List[String] = {
      val parts = path.split(java.io.File.separatorChar).filterNot(_.isEmpty)
      (0 to parts.length)
        .map(i => s"${java.io.File.separatorChar}${parts.take(i).mkString(java.io.File.separator)}")
        .toList
    }

    val paths        = parentPaths(cwd)
    val pathsToCheck = homeDirOpt :: homeDirOpt :: paths

    // Use ZIO to filter the paths
    for {
      doPathExist  <- ZIO.foreach(pathsToCheck)(path => ZIO.succeed(Files.exists(Path.of(path, filename))))
      existingPaths = doPathExist.zip(pathsToCheck).collect { case (exists, path) if exists => path }
    } yield existingPaths.distinct // Use distinct to remove duplicates at the end
  }

  def loadOptionsFromConfigFiles(topLevelCommand: String): ZIO[Any, IOException, List[String]] =
    for {
      filePaths <- findPathsOfCliConfigFiles(topLevelCommand)
                     .refineToOrDie[IOException]
      lines <- ZIO
                 .foreach(filePaths) { filePath =>
                   ZIO.acquireReleaseWith(
                     ZIO.attempt(
                       Source.fromFile(Paths.get(filePath, "." + topLevelCommand).toFile)
                     )
                   )(source => ZIO.attempt(source.close()).orDie) { source =>
                     ZIO.attempt(source.getLines().toList.filter(_.trim.nonEmpty))
                   }
                 }
                 .map(_.flatten)
                 .refineToOrDie[IOException]
    } yield lines
}
