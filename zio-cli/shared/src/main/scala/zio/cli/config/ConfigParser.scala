package zio.cli.config

/**
 * Represents a single configuration option parsed from a dotfile.
 *
 * @param key      The option key, including dashes (e.g. "--max-lines")
 * @param value    Optional value for the option
 * @param source   File path string where the option was found
 * @param priority Priority index (higher = more important, closer to cwd)
 */
case class ConfigOption(
  key: String,
  value: Option[String],
  source: String,
  priority: Int
)

/**
 * Parses dotfile lines into structured [[ConfigOption]] values.
 *
 * Supported formats per line:
 *   - `--key=value`
 *   - `--flag`
 *   - Lines starting with `#` are treated as comments
 *   - Empty lines are ignored
 */
object ConfigParser {

  def parseLines(lines: List[String], source: String, priority: Int): List[ConfigOption] =
    lines
      .map(_.trim)
      .filterNot(l => l.isEmpty || l.startsWith("#"))
      .flatMap(line => parseLine(line, source, priority))

  private[config] def parseLine(line: String, source: String, priority: Int): Option[ConfigOption] =
    if (line.startsWith("-")) {
      val eqIdx = line.indexOf('=')
      if (eqIdx > 0) {
        val key   = line.substring(0, eqIdx).trim
        val value = line.substring(eqIdx + 1).trim
        Some(ConfigOption(key, Some(value), source, priority))
      } else {
        Some(ConfigOption(line.trim, None, source, priority))
      }
    } else None
}
