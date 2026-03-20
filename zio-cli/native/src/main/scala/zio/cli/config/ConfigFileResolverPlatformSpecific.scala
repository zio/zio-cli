package zio.cli.config

import zio._
import java.nio.file.{Files, Path, Paths}
import scala.io.Source

/**
 * Native implementation of config file resolution. Identical to JVM — Scala Native supports `java.nio.file`.
 */
trait ConfigFileResolverPlatformSpecific {

  def resolveAndParse(commandName: String): Task[List[ConfigOption]] =
    ZIO.attempt {
      val fileName = s".$commandName"
      val homePath = sys.props.get("user.home").map(Paths.get(_)).map(_.resolve(fileName))
      val cwd      = Paths.get("").toAbsolutePath

      def getParents(path: Path): List[Path] = {
        val parent = path.getParent
        if (parent == null) List(path) else path :: getParents(parent)
      }

      val cwdUpToRoot = getParents(cwd).reverse
      val cwdPaths    = cwdUpToRoot.map(_.resolve(fileName))
      val allPaths    = homePath.toList ++ cwdPaths
      val existing    = allPaths.distinct.filter(Files.exists(_))

      existing.zipWithIndex.flatMap { case (path, priority) =>
        val source = Source.fromFile(path.toFile, "UTF-8")
        val lines  =
          try source.getLines().toList
          finally source.close()
        ConfigParser.parseLines(lines, path.toString, priority)
      }
    }
}
