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
  def completions(shellType: ShellType): String = ???

  def helpDoc: HelpDoc = ???

  def run(args: List[String]): ZIO[R with console.Console, Nothing, ExitCode] =
    (for {
      validationResult <- command.validate(args, options)
      (args, a, b)     = validationResult
      result           <- command.execute(a, b) // Hmm shouldn't we still thread through the remaining args
    } yield result).exitCode

}
