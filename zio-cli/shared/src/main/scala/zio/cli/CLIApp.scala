package zio.cli

import zio._

import zio.cli.HelpDoc.{ h1, p }
import zio.cli.HelpDoc.Span.text

/**
 * A `CLIApp[R, E]` is a complete description of a command-line application, which
 * requires environment `R`, and may fail with a value of type `E`.
 */
sealed trait CLIApp[-R, +E] {
  type Model

  def name: String
  def version: String
  def command: Command[Model]
  def summary: HelpDoc.Span
  def footer: HelpDoc
  def options: ParserOptions

  def execute(model: Model): ZIO[R, E, Any]

  def completions(shellType: ShellType): String = ???

  def helpDoc: HelpDoc =
    h1(text(command.name) + text(" -- ") + text(version)) +
      p(text(command.name) + text(" -- ") + summary) +
      generateDoc(command) +
      footer

  def run(args: List[String]): ZIO[R with console.Console, Nothing, ExitCode] = {
    val c = command

    (for {
      validationResult <- c.validate(args, options)
      (_, opts, args)  = validationResult
      m                = c.output(opts, args)
      result           <- execute(m)
    } yield result).exitCode
  }

  private def generateDoc(command: Command[_]): HelpDoc = {
    val descriptionSection =
      if (command.description != HelpDoc.Empty)
        (h1("description") +
          command.description)
      else HelpDoc.Empty

    val argumentsSection = {
      val args = command.args.helpDoc

      if (args == HelpDoc.Empty) HelpDoc.Empty
      else h1("arguments") + command.args.helpDoc
    }

    val optionsSection = {
      val opts = command.options.helpDoc

      if (opts == HelpDoc.Empty) HelpDoc.Empty
      else h1("options") + command.options.helpDoc
    }

    descriptionSection +
      argumentsSection +
      optionsSection
  }

}
object CLIApp {
  def apply[R, E, M](
    name0: String,
    version0: String,
    command0: Command[M],
    summary0: HelpDoc.Span,
    execute0: M => ZIO[R, E, Any] = (_: M) => ZIO.unit,
    footer0: HelpDoc = HelpDoc.Empty,
    options0: ParserOptions = ParserOptions.default
  ): CLIApp[R, E] =
    new CLIApp[R, E] {
      type Model = M

      def name                                  = name0
      def version                               = version0
      def command                               = command0
      def summary                               = summary0
      def footer                                = footer0
      def options                               = options0
      def execute(model: Model): ZIO[R, E, Any] = execute0(model)
    }
}
