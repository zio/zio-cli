package zio.cli

import zio.IO

final case class BuiltIn(help: Boolean, shellCompletions: Option[ShellType])
object BuiltIn {
  trait BuiltInOptions {
    lazy val builtInOptions: Options[BuiltIn] =
      (Options.bool("help", true) :: ShellType.option.optional("N/A")).as(BuiltIn(_, _))

    final def parseBuiltIn(args: List[String], opts: ParserOptions): IO[HelpDoc, (List[String], BuiltIn)] =
      builtInOptions.validate(args, opts)
  }
}
