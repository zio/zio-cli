package zio.cli

import zio.{URIO, ZIO}

/**
 * A `CliConfig` sets how ZIO CLI parses the command line and how the help screen shows the usage line. Pass one to
 * `CliApp.make` or `CliApp#config`; `CliConfig.default` is used otherwise.
 *
 * {{{
 * val strict: CliConfig = CliConfig.default.copy(caseSensitive = true, autoCorrectLimit = 1)
 * }}}
 *
 * @param caseSensitive
 *   whether names are compared with their case. When `false`, command names and boolean values such as `YES` match in
 *   any case; an option name typed in a different case than declared is still not read (`--NAME` is not read as
 *   `--name`).
 * @param autoCorrectLimit
 *   the largest edit distance between a mistyped option and a declared option name for which a parse error suggests the
 *   declared name, as in `The flag "--nmae" is not recognized. Did you mean --name?`. Only option names longer than
 *   `autoCorrectLimit + 1` characters get this suggestion. The suggestion is shown only when parsing then fails:
 *   `Command("c", Options.text("name").optional)` parses `c --nmae x` as `None` without an error.
 * @param finalCheckBuiltIn
 *   whether, when parsing fails, to look for `--help`, `-h`, `--wizard` or `-w` anywhere in the input and show the help
 *   or the wizard instead of the error.
 * @param showAllNames
 *   whether the usage line of the help screen shows every name of an option; when `false`, it shows only the first long
 *   name, or every name when the option has no long name. The options section always shows every name.
 * @param showTypes
 *   whether the usage line of the help screen shows the value placeholders of options, such as `<text>`. The options
 *   section always shows them.
 *
 * @see
 *   [[https://zio.dev/zio-cli/cli-config/ CliConfig]]
 */
final case class CliConfig(
  caseSensitive: Boolean,
  autoCorrectLimit: Int,
  finalCheckBuiltIn: Boolean = true,
  showAllNames: Boolean = true,
  showTypes: Boolean = true
) {

  /**
   * Returns `text` in lower case, or unchanged when `caseSensitive` is `true`.
   */
  def normalizeCase(text: String): String = if (caseSensitive) text else text.toLowerCase()

  /**
   * Returns `true` when `value`, without surrounding spaces, is `--` alone or `--` followed by exactly one character
   * other than `-`. A longer name such as `--name` gives `false`.
   */
  def isLongOption(value: String): Boolean = value.trim.matches("^-{2}([^-]|$)")

  /**
   * Returns `true` when `value`, without surrounding spaces, is `-` alone or `-` followed by exactly one character
   * other than `-`, such as `-v`. `-vv` gives `false`.
   */
  def isShortOption(value: String): Boolean = value.trim.matches("^-{1}([^-]|$)")

  /**
   * Returns `true` when `isLongOption` or `isShortOption` does.
   */
  def isOption(value: String): Boolean = isLongOption(value) || isShortOption(value)
}

/**
 * The default configuration, and access to a configuration in the ZIO environment.
 */
object CliConfig {

  /**
   * The default configuration: case-insensitive, suggestions for option names up to edit distance 2, the final built-in
   * check on, and every name and type shown in the usage line.
   */
  val default: CliConfig = CliConfig(caseSensitive = false, autoCorrectLimit = 2, finalCheckBuiltIn = true)

  /**
   * Reads the `CliConfig` from the ZIO environment.
   */
  val cliConfig: URIO[CliConfig, CliConfig] = ZIO.service
}
