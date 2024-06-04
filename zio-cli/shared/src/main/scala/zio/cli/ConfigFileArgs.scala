package zio.cli

import zio._
import java.io.IOException

trait ConfigFilePlatformSpecific {
  def findPathsOfCliConfigFiles(topLevelCommand: String): Task[List[String]]
  def loadOptionsFromConfigFiles(topLevelCommand: String): ZIO[Any, IOException, List[String]]
}
