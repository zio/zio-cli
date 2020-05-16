package zio.cli

/**
 * A `ParserOptions` describes how arguments from the command-line are to
 * be parsed by ZIO CLI.
 *
 * @param caseSensitive    Whether or not to be case sensitive.
 * @param defaultFormat    The default format of options, e.g. kebab case.
 * @param supportedFormats Alternative formats to support for options, if any.
 */
final case class ParserOptions(
  caseSensitive: Boolean,
  defaultFormat: OptionFormat,
  supportedFormats: Set[OptionFormat]
)

object ParserOptions {

  /**
   * The default options are case sensitive parsing, kebab casing for options,
   * and no alternate formats to support for options.
   */
  val default: ParserOptions = ParserOptions(true, OptionFormat.Kebab, Set())
}
