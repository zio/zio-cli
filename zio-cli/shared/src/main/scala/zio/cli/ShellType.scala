package zio.cli

sealed trait ShellType 
object ShellType {
  case object Bash extends ShellType 
  case object ZShell extends ShellType
}