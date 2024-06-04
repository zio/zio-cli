package zio.cli

import java.io.IOException
import zio._

object ConfigFileArgsPlatformSpecific extends ConfigFilePlatformSpecific {
  def findPathsOfCliConfigFiles(topLevelCommand: String): Task[List[String]] =
    ZIO.succeed(Nil) // Always return empty for JS

  def loadOptionsFromConfigFiles(topLevelCommand: String): ZIO[Any, IOException, List[String]] =
    ZIO.succeed(Nil) // Always return empty for JS
}
