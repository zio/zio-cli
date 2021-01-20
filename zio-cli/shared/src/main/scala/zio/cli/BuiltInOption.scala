package zio.cli

sealed trait BuiltInOption
object BuiltInOption {
  final case class ShowHelp(helpDoc: HelpDoc)                      extends BuiltInOption
  final case class ShowCompletions(completions: Set[List[String]]) extends BuiltInOption

  def builtInOptions(helpDoc: => HelpDoc, completions: ShellType => Set[List[String]]): Options[Option[BuiltInOption]] =
    (Options.bool("help", true) :: ShellType.option.optional("N/A")).flatten2.map {
      case (true, _)            => Some(ShowHelp(helpDoc))
      case (_, Some(shellType)) => Some(ShowCompletions(completions(shellType)))
      case _                    => None
    }
}
