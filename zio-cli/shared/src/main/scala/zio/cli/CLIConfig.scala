package zio.cli

/**
 * A `CLIConfig` describes how arguments from the command-line are to
 * be parsed by ZIO CLI.
 *
 * @param caseSensitive     Whether or not to be case sensitive.
 * @param autoCorrectLimit  Threshold for when to show auto correct suggestions
 */
final case class CLIConfig(
  caseSensitive: Boolean,
  autoCorrectLimit: Int
) {
  def normalizeCase(text: String): String = if (caseSensitive) text else text.toLowerCase()

  def isLongOption(value: String): Boolean = value.trim.matches("^-{2}([^-]|$)")

  def isShortOption(value: String): Boolean = value.trim.matches("^-{1}([^-]|$)")

  def isOption(value: String): Boolean = isLongOption(value) || isShortOption(value)
}

object CLIConfig {

  /**
   * The default options are case sensitive parsing
   */
  val default: CLIConfig = CLIConfig(false, 2)
}
