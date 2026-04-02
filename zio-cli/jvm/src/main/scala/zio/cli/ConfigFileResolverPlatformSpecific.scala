package zio.cli

import zio._

import java.nio.file.{Files => JFiles, Path => JPath, Paths => JPaths}

private[cli] trait ConfigFileResolverPlatformSpecific {

  lazy val live: ConfigFileResolver = new ConfigFileResolver {

    def resolve(appName: String): UIO[(List[String], List[SettingSource])] =
      ZIO.attempt {
        val dotFileName = s".$appName"
        val cwd         =
          JPaths.get(java.lang.System.getProperty("user.dir")).toAbsolutePath.normalize()

        val files        = collectDotFiles(cwd, dotFileName)
        val orderedFiles = files.reverse

        orderedFiles.foldLeft((List.empty[String], List.empty[SettingSource])) { case ((accArgs, accSources), file) =>
          val content       = new String(JFiles.readAllBytes(file), "UTF-8")
          val filePath      = file.toString
          val (fileArgs, _) = ConfigFileResolver.parseDotFile(content, filePath)
          ConfigFileResolver.mergeArgs(accArgs, accSources, fileArgs)
        }
      }
        .catchAll(_ => ZIO.succeed((Nil, Nil)))

    private def collectDotFiles(startDir: JPath, dotFileName: String): List[JPath] = {
      var result     = List.empty[JPath]
      var dir: JPath = startDir
      while (dir != null) {
        val dotFile = dir.resolve(dotFileName)
        if (JFiles.isRegularFile(dotFile) && JFiles.isReadable(dotFile)) {
          result = dotFile :: result
        }
        dir = dir.getParent
      }
      result.reverse
    }
  }
}
