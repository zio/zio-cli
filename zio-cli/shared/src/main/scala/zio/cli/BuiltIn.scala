package zio.cli

import zio.IO

final case class BuiltIn(help: Boolean, shellCompletions: Option[ShellType])
object BuiltIn {
  trait BuiltInOptions[+A] {
    lazy val builtInOptions: Options[BuiltIn] =
      (Options.bool("help", default = false) :: ShellType.option.optional("N/A")).as(BuiltIn(_, _))

    final def parseBuiltIn(args: List[String], opts: ParserOptions): IO[List[HelpDoc], (List[String], BuiltIn)] =
      builtInOptions.validate(args, opts)
  }
}
