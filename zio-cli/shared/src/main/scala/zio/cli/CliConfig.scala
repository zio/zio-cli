package zio.cli

import zio.{URIO, ZIO}

/**
 * A `CliConfig` describes how arguments from the command-line are to be parsed by ZIO CLI.
 *
 * @param caseSensitive
 *   Whether or not to be case sensitive.
 * @param autoCorrectLimit
 *   Threshold for when to show auto correct suggestions.
 * @param finalCheckBuiltIn
 *   Whether or not to check for a BuiltIn option even if it is not a valid command.
 * @param showAllNames
 *   Whether or not to show all the names of an option in the synopsis of a command.
 * @param showTypes
 *   Whether or not to show the type of an option in the synopsis of a command.
 */
final case class CliConfig(
  caseSensitive: Boolean,
  autoCorrectLimit: Int,
  finalCheckBuiltIn: Boolean = true,
  showAllNames: Boolean = true,
  showTypes: Boolean = true
) {
  def normalizeCase(text: String): String = if (caseSensitive) text else text.toLowerCase()

  def isLongOption(value: String): Boolean = value.trim.matches("^-{2}([^-]|$)")

  def isShortOption(value: String): Boolean = value.trim.matches("^-{1}([^-]|$)")

  def isOption(value: String): Boolean = isLongOption(value) || isShortOption(value)
}

object CliConfig {

  /**
   * The default options are case sensitive parsing
   */
  val default: CliConfig = CliConfig(caseSensitive = false, autoCorrectLimit = 2, finalCheckBuiltIn = true)

  val cliConfig: URIO[CliConfig, CliConfig] = ZIO.service
}
