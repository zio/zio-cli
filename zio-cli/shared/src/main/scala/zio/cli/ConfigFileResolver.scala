package zio.cli

import zio._

/**
 * Represents a setting value along with where it came from.
 */
final case class SettingSource(name: String, value: String, source: String)

/**
 * Resolves CLI options from dotfiles by walking the directory tree from CWD upward, looking for `.<appname>` files.
 * Files closer to CWD override settings from parent directories. CLI arguments override all file settings.
 *
 * Dotfile format: one option per line, using the same syntax as CLI arguments. Lines starting with `#` are treated as
 * comments. Empty lines are ignored.
 */
trait ConfigFileResolver {

  /**
   * Resolves configuration from dotfiles for the given app name. Returns a list of args (in CLI format) from dotfiles,
   * ordered so that closer files override parent files, along with provenance information for diagnostics.
   */
  def resolve(appName: String): UIO[(List[String], List[SettingSource])]
}

object ConfigFileResolver extends ConfigFileResolverPlatformSpecific {

  /**
   * A resolver that returns no configuration (used for JS or when dotfile support is disabled).
   */
  val none: ConfigFileResolver = new ConfigFileResolver {
    def resolve(appName: String): UIO[(List[String], List[SettingSource])] =
      ZIO.succeed((Nil, Nil))
  }

  /**
   * Parses dotfile content into a list of CLI-style arguments and provenance info.
   */
  private[cli] def parseDotFile(
    content: String,
    filePath: String
  ): (List[String], List[SettingSource]) = {
    val lines = content.split('\n').toList

    val (args, sources) =
      lines.foldLeft((List.empty[String], List.empty[SettingSource])) { case ((accArgs, accSources), line) =>
        val trimmed = line.trim
        if (trimmed.isEmpty || trimmed.startsWith("#"))
          (accArgs, accSources)
        else {
          val tokens     = tokenizeLine(trimmed)
          val newSources = tokens match {
            case name :: value :: _ if name.startsWith("-") =>
              List(SettingSource(name, value, filePath))
            case name :: Nil if name.startsWith("-") =>
              List(SettingSource(name, "", filePath))
            case _ =>
              Nil
          }
          (accArgs ++ tokens, accSources ++ newSources)
        }
      }
    (args, sources)
  }

  /**
   * Tokenizes a single line into CLI argument tokens. Handles quoted values.
   */
  private[cli] def tokenizeLine(line: String): List[String] = {
    val tokens                = scala.collection.mutable.ListBuffer.empty[String]
    val current               = new StringBuilder
    var inQuote: Option[Char] = None
    var i                     = 0

    while (i < line.length) {
      val c = line.charAt(i)
      inQuote match {
        case Some(q) =>
          if (c == q) inQuote = None
          else current.append(c)
        case None =>
          if (c == '"' || c == '\'') inQuote = Some(c)
          else if (c == ' ' || c == '\t') {
            if (current.nonEmpty) {
              tokens += current.toString()
              current.clear()
            }
          } else current.append(c)
      }
      i += 1
    }
    if (current.nonEmpty) tokens += current.toString()
    tokens.toList
  }

  /**
   * Merges dotfile args with CLI args. CLI args take precedence. For each option found in CLI args, remove it from the
   * dotfile args. Returns the merged args and updated provenance info.
   */
  private[cli] def mergeArgs(
    dotfileArgs: List[String],
    dotfileSources: List[SettingSource],
    cliArgs: List[String]
  ): (List[String], List[SettingSource]) = {
    val cliOptionNames = extractOptionNames(cliArgs)

    val filteredDotfileArgs = filterOverriddenArgs(dotfileArgs, cliOptionNames)
    val filteredSources     =
      dotfileSources.filterNot(s => cliOptionNames.contains(normalizeOptionName(s.name)))

    val cliSources = extractCliSources(cliArgs)

    (filteredDotfileArgs ++ cliArgs, filteredSources ++ cliSources)
  }

  private[cli] def extractOptionNamesFromArgs(args: List[String]): Set[String] =
    args.filter(_.startsWith("-")).map(normalizeOptionName).toSet

  private def extractOptionNames(args: List[String]): Set[String] =
    extractOptionNamesFromArgs(args)

  private[cli] def normalizeOptionName(name: String): String =
    name.takeWhile(_ != '=').toLowerCase

  private[cli] def filterOverriddenFromArgs(
    args: List[String],
    overriddenNames: Set[String]
  ): List[String] = filterOverriddenArgs(args, overriddenNames)

  private def filterOverriddenArgs(
    args: List[String],
    overriddenNames: Set[String]
  ): List[String] = {
    var result    = List.empty[String]
    var remaining = args
    while (remaining.nonEmpty) {
      remaining match {
        case name :: value :: rest if name.startsWith("-") && !value.startsWith("-") =>
          if (!overriddenNames.contains(normalizeOptionName(name)))
            result = result :+ name :+ value
          remaining = rest
        case name :: rest if name.startsWith("-") =>
          if (!overriddenNames.contains(normalizeOptionName(name)))
            result = result :+ name
          remaining = rest
        case other :: rest =>
          result = result :+ other
          remaining = rest
        case Nil =>
          remaining = Nil
      }
    }
    result
  }

  private def extractCliSources(args: List[String]): List[SettingSource] = {
    var result    = List.empty[SettingSource]
    var remaining = args
    while (remaining.nonEmpty) {
      remaining match {
        case name :: value :: rest if name.startsWith("-") && !value.startsWith("-") =>
          result = result :+ SettingSource(name, value, "<cli>")
          remaining = rest
        case name :: rest if name.startsWith("-") =>
          result = result :+ SettingSource(name, "", "<cli>")
          remaining = rest
        case _ :: rest =>
          remaining = rest
        case Nil =>
          remaining = Nil
      }
    }
    result
  }

  /**
   * Convert a flat list of CLI arg tokens into a map of option-name -> values for use with Options.validate.
   */
  private[cli] def argsToOptionMap(args: List[String]): Predef.Map[String, List[String]] = {
    @scala.annotation.tailrec
    def loop(
      remaining: List[String],
      acc: Predef.Map[String, List[String]]
    ): Predef.Map[String, List[String]] =
      remaining match {
        case Nil                                  => acc
        case head :: tail if head.startsWith("-") =>
          if (head.contains("=")) {
            val splitAt = head.indexOf('=')
            val key     = head.substring(0, splitAt)
            val value   = head.substring(splitAt + 1)
            loop(tail, acc.updated(key, acc.getOrElse(key, Nil) :+ value))
          } else if (tail.nonEmpty && !tail.head.startsWith("-")) {
            loop(tail.tail, acc.updated(head, acc.getOrElse(head, Nil) :+ tail.head))
          } else {
            loop(tail, acc.updated(head, Nil))
          }
        case _ :: tail =>
          loop(tail, acc)
      }

    loop(args, Predef.Map.empty)
  }

  /**
   * Format diagnostics about where settings came from.
   */
  def formatDiagnostics(sources: List[SettingSource]): String =
    if (sources.isEmpty) "No settings loaded."
    else {
      val maxNameLen = sources.map(_.name.length).max
      sources.map { s =>
        val valueStr = if (s.value.nonEmpty) s" = ${s.value}" else ""
        val padding  = " " * (maxNameLen - s.name.length)
        s"  ${s.name}$padding$valueStr  (from ${s.source})"
      }
        .mkString("Settings:\n", "\n", "")
    }
}
