package zio.cli.config

import zio._

import java.io.File
import scala.annotation.tailrec
import scala.io.Source

private[cli] trait ConfigFileResolverPlatformSpecific {

  def resolveAndParse(commandName: String): Task[List[ConfigOption]] =
    ZIO.attempt {
      val fileName = s".$commandName"
      val homeFile = Option(System.getProperty("user.home")).map(home => new File(home, fileName)).toList
      val cwd      = new File(Option(System.getProperty("user.dir")).getOrElse(".")).getAbsoluteFile

      val candidates = distinctByPath(homeFile ++ directoriesFromRootTo(cwd).map(new File(_, fileName)))
        .filter(file => file.exists() && file.isFile)

      candidates.zipWithIndex.flatMap { case (file, priority) =>
        val source = Source.fromFile(file, "UTF-8")
        val lines  = try source.getLines().toList finally source.close()
        ConfigParser.parseLines(lines, file.getAbsolutePath, priority)
      }
    }

  private def distinctByPath(files: List[File]): List[File] = {
    val seen = scala.collection.mutable.HashSet.empty[String]
    files.filter { file =>
      val path = file.getAbsolutePath
      if (seen.contains(path)) false
      else {
        seen += path
        true
      }
    }
  }

  private def directoriesFromRootTo(cwd: File): List[File] = {
    @tailrec
    def loop(current: File, acc: List[File]): List[File] =
      if (current == null) acc
      else loop(current.getParentFile, current :: acc)

    loop(cwd, Nil)
  }
}
