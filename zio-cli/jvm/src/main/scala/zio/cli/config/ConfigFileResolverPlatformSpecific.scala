package zio.cli.config

import zio._
import java.nio.file.{Files, Path, Paths}
import scala.io.Source

/**
 * JVM implementation of config file resolution.
 * Walks from CWD to root, then home, looking for `.<commandName>` dotfiles.
 */
trait ConfigFileResolverPlatformSpecific {

  /**
   * Discovers config files and parses them into [[ConfigOption]] values.
   * Priority order: home (lowest) → root → … → cwd (highest).
   */
  def resolveAndParse(commandName: String): Task[List[ConfigOption]] =
    ZIO.attempt {
      val fileName = s".$commandName"
      val homePath = sys.props.get("user.home").map(Paths.get(_)).map(_.resolve(fileName))
      val cwd      = Paths.get("").toAbsolutePath

      def getParents(path: Path): List[Path] = {
        val parent = path.getParent
        if (parent == null) List(path) else path :: getParents(parent)
      }

      // From root to CWD
      val cwdUpToRoot = getParents(cwd).reverse
      val cwdPaths    = cwdUpToRoot.map(_.resolve(fileName))

      // home first (lowest priority), then root→cwd
      val allPaths = homePath.toList ++ cwdPaths

      // De-duplicate, keep only existing files, and assign priority indices
      val existing = allPaths.distinct.filter(Files.exists(_))

      existing.zipWithIndex.flatMap { case (path, priority) =>
        val source = Source.fromFile(path.toFile, "UTF-8")
        val lines  = try source.getLines().toList finally source.close()
        ConfigParser.parseLines(lines, path.toString, priority)
      }
    }
}
