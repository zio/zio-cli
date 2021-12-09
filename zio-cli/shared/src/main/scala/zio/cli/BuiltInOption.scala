package zio.cli

import java.nio.file.{Path => JPath}

sealed trait BuiltInOption extends Product with Serializable
object BuiltInOption {
  final case class ShowHelp(helpDoc: HelpDoc)                                          extends BuiltInOption
  final case class ShowCompletionScript(pathToExecutable: JPath, shellType: ShellType) extends BuiltInOption
  final case class ShowCompletions(index: Int, shellType: ShellType)                   extends BuiltInOption

  final case class BuiltIn(
    help: Boolean,
    shellCompletionScriptPath: Option[JPath],
    shellType: Option[ShellType],
    shellCompletionIndex: Option[Int]
  )

  lazy val builtInOptions = (
    Options.boolean("help") ++
      Options.file("shell-completion-script").optional("N/A") ++
      ShellType.option.optional("N/A") ++
      Options.integer("shell-completion-index").map(_.toInt).optional("N/A")
  ).as(BuiltIn.apply _)

  def builtInOptions(helpDoc: => HelpDoc): Options[Option[BuiltInOption]] =
    builtInOptions.map {
      case BuiltIn(true, _, _, _)                      => Some(ShowHelp(helpDoc))
      case BuiltIn(_, Some(path), Some(shellType), _)  => Some(ShowCompletionScript(path, shellType))
      case BuiltIn(_, _, Some(shellType), Some(index)) => Some(ShowCompletions(index, shellType))
      case _                                           => None
    }
}
