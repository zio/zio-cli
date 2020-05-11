package zio.cli

sealed trait Command[+A]

object Command {
  case object Process extends Command[Unit]
  sealed case class Sub[A](parent: Command[A]) extends Command[A]
}
