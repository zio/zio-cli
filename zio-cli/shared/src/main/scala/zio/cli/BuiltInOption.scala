package zio.cli

sealed trait BuiltInOption
object BuiltInOption {
  final case class ShowHelp(helpDoc: HelpDoc)                      extends BuiltInOption
  final case class ShowCompletions(completions: Set[List[String]]) extends BuiltInOption

  final case class BuiltIn(help: Boolean, shellCompletions: Option[ShellType])

  lazy val builtInOptions: Options[BuiltIn] =
    (Options.boolean("help", ifPresent = true) ++ ShellType.option.optional("N/A")).as(BuiltIn)

  def builtInOptions(helpDoc: => HelpDoc, completions: ShellType => Set[List[String]]): Options[Option[BuiltInOption]] =
    builtInOptions.map {
      case BuiltIn(true, _)            => Some(ShowHelp(helpDoc))
      case BuiltIn(_, Some(shellType)) => Some(ShowCompletions(completions(shellType)))
      case _                           => None
    }
}
