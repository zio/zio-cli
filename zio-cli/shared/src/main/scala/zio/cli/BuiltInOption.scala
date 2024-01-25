package zio.cli

sealed trait BuiltInOption extends Product with Serializable
object BuiltInOption       extends PathPlatformSpecific {
  // TODO add built-in option to show version
  final case class ShowHelp(synopsis: UsageSynopsis, helpDoc: HelpDoc)                 extends BuiltInOption
  final case class ShowCompletionScript(pathToExecutable: JPath, shellType: ShellType) extends BuiltInOption
  final case class ShowCompletions(index: Int, shellType: ShellType)                   extends BuiltInOption
  final case class ShowWizard(command: Command[_])                                     extends BuiltInOption

  final case class BuiltIn(
    help: Boolean,
    shellCompletionScriptPath: Option[JPath],
    shellType: Option[ShellType],
    shellCompletionIndex: Option[Int],
    wizard: Boolean
  )

  def builtInOptions(
    command: => Command[_],
    usageSynopsis: => UsageSynopsis,
    helpDoc: => HelpDoc
  ): Options[Option[BuiltInOption]] = {
    val options = (
      Options.boolean("help").alias("h") ++
        Options.file("shell-completion-script").optional ++
        ShellType.option.optional ++
        Options.integer("shell-completion-index").map(_.toInt).optional ++
        Options.boolean("wizard")
    ).as(BuiltIn.apply _)

    options.map {
      case BuiltIn(true, _, _, _, _)                      => Some(ShowHelp(usageSynopsis, helpDoc))
      case BuiltIn(_, _, _, _, true)                      => Some(ShowWizard(command))
      case BuiltIn(_, Some(path), Some(shellType), _, _)  => Some(ShowCompletionScript(path, shellType))
      case BuiltIn(_, _, Some(shellType), Some(index), _) => Some(ShowCompletions(index, shellType))
      case _                                              => None
    }
  }
}
