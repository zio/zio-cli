package zio.cli

final case class BuiltIn(help: Boolean, shellCompletions: Option[ShellType])
object BuiltIn {
  val options: Options[BuiltIn] =
    (Options.bool("help", true) :: ShellType.option.optional("N/A")).as(BuiltIn(_, _))
}
