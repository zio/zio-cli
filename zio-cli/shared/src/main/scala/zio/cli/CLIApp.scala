package zio.cli

import zio._

/**
 * A `CLIApp[R, E]` is a complete description of a command-line application, which
 * requires environment `R`, and may fail with a value of type `E`.
 */
sealed trait CLIApp[-R, +E] {
  type Model

  def name: String
  def version: String
  def command: Command[Model]
  def options: ParserOptions

  def execute(model: Model): ZIO[R, E, Any]

  def completions(shellType: ShellType): String = ???

  def helpDoc: HelpDoc = ???

  def run(args: List[String]): ZIO[R with console.Console, Nothing, ExitCode] = {
    val c = command

    (for {
      validationResult <- c.validate(args, options)
      (_, opts, args)  = validationResult
      m                = c.output(opts, args)
      result           <- execute(m)
    } yield result).exitCode
  }

}
object CLIApp {
  def apply[R, E, M](
    name0: String,
    version0: String,
    command0: Command[M],
    execute0: M => ZIO[R, E, Any] = (_: M) => ZIO.unit,
    options0: ParserOptions = ParserOptions.default
  ): CLIApp[R, E] =
    new CLIApp[R, E] {
      type Model = M

      def name                                  = name0
      def version                               = version0
      def command                               = command0
      def options                               = options0
      def execute(model: Model): ZIO[R, E, Any] = execute0(model)
    }
}
