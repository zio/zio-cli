package zio.cli

sealed trait ShellType
object ShellType {
  case object Bash   extends ShellType
  case object ZShell extends ShellType

  val option: Options[ShellType] =
    Options.enumeration("shell-type")(
      "sh"   -> Bash,
      "bash" -> Bash,
      "zsh"  -> ZShell
    )
}
