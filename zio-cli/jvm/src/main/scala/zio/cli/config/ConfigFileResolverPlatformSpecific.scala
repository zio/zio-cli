package zio.cli.config

import zio._
import java.io.File
import scala.io.Source

/**
 * JVM implementation of config file resolution. Walks from CWD to root, then home, looking for `.<commandName>`
 * dotfiles.
 */
trait ConfigFileResolverPlatformSpecific {

  /**
   * Discovers config files and parses them into [[ConfigOption]] values. Priority order: home (lowest) → root → … → cwd
   * (highest).
   */
  def resolveAndParse(commandName: String): Task[List[ConfigOption]] =
    ZIO.attempt {
      val fileName = s".$commandName"
      val homePath = sys.props.get("user.home").map(new File(_, fileName))
      val cwd      = new File("").getAbsoluteFile

      def getParents(file: File): List[File] = {
        val parent = file.getParentFile
        if (parent == null) List(file) else file :: getParents(parent)
      }

      // From root to CWD
      val cwdUpToRoot = getParents(cwd).reverse
      val cwdPaths    = cwdUpToRoot.map(dir => new File(dir, fileName))

      // home first (lowest priority), then root→cwd
      val allPaths = homePath.toList ++ cwdPaths

      // De-duplicate, keep only existing files, and assign priority indices
      val existing = allPaths.distinct.filter(_.exists())

      existing.zipWithIndex.flatMap { case (file, priority) =>
        val source = Source.fromFile(file, "UTF-8")
        val lines  =
          try source.getLines().toList
          finally source.close()
        ConfigParser.parseLines(lines, file.getAbsolutePath, priority)
      }
    }
}
