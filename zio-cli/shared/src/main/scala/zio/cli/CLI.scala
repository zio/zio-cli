package zio.cli

import zio._

/**
 * A `CLIApp[R, E]` is a complete description of a command-line application, which
 * requires environment `R`, and may fail with a value of type `E`.
 */
final case class CLIApp[-R, +E](
  name: String,
  version: String,
  command: Command[R, E],
  options: ParserOptions = ParserOptions.default
) {
  def run(args: List[String]): ZIO[R, Nothing, Int] = ???

  def helpDoc: HelpDoc = ???
}
