package zio.cli.config

import zio._
import java.io.File
import scala.io.Source

/**
 * Native implementation of config file resolution. Uses java.io.File which is much more stable in Scala Native than
 * java.nio.file.
 */
trait ConfigFileResolverPlatformSpecific {

  def resolveAndParse(commandName: String): Task[List[ConfigOption]] =
    ZIO.attempt {
      val fileName = s".$commandName"
      val homePath = sys.props.get("user.home").map(new File(_, fileName))
      val cwd      = new File("").getAbsoluteFile

      def getParents(file: File): List[File] = {
        val parent = file.getParentFile
        if (parent == null) List(file) else file :: getParents(parent)
      }

      val cwdUpToRoot = getParents(cwd).reverse
      val cwdPaths    = cwdUpToRoot.map(dir => new File(dir, fileName))
      val allPaths    = homePath.toList ++ cwdPaths
      val existing    = allPaths.distinct.filter(_.exists())

      existing.zipWithIndex.flatMap { case (file, priority) =>
        val source = Source.fromFile(file, "UTF-8")
        val lines  =
          try source.getLines().toList
          finally source.close()
        ConfigParser.parseLines(lines, file.getAbsolutePath, priority)
      }
    }
}
