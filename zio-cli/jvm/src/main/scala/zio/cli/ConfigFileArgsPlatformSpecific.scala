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
      do_path_exist <- ZIO.foreach(pathsToCheck)(path => ZIO.succeed(Files.exists(Path.of(path, filename))))
      existing_paths = do_path_exist.zip(pathsToCheck).collect { case (exists, path) if exists => path }
    } yield existing_paths.distinct // Use distinct to remove duplicates at the end
  }

  def mergeOptionsBasedOnPriority(options: List[String]): List[String] = {
    val mergedOptions = options.flatMap { opt =>
      opt.split('=') match {
        case Array(key)        => Some(key -> None)
        case Array(key, value) => Some(key -> value)
        case _ =>
          None // handles the case when there isn't exactly one '=' in the string
      }
    }.toMap.toList.map {
      case (key, None)  => key
      case (key, value) => s"$key=$value"
    }

    mergedOptions
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
