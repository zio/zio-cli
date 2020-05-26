package zio.cli

/**
 * A `ParserOptions` describes how arguments from the command-line are to
 * be parsed by ZIO CLI.
 *
 * @param caseSensitive     Whether or not to be case sensitive.
 */
final case class ParserOptions(
  caseSensitive: Boolean
) {
  def normalizeCase(text: String): String = if (caseSensitive) text else text.toLowerCase()
}

object ParserOptions {

  /**
   * The default options are case sensitive parsing
   */
  val default: ParserOptions = ParserOptions(true)
}
