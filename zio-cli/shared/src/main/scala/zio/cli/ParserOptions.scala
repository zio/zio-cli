package zio.cli

final case class ParserOptions(
  caseSensitive: Boolean,
  defaultFormat: OptionFormat,
  supportedFormats: Set[OptionFormat]
)

object ParserOptions {
  val default: ParserOptions = ParserOptions(true, OptionFormat.Kebab, Set())
}
