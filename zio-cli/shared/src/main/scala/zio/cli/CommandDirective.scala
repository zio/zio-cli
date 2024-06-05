package zio.cli

sealed trait CommandDirective[+A] { self =>
  import CommandDirective._

  def map[B](f: A => B): CommandDirective[B] =
    self match {
      case x @ BuiltIn(_)               => x
      case UserDefined(leftover, value) => CommandDirective.UserDefined(leftover, f(value))
    }

  def mapBuiltIn(f: BuiltInOption => BuiltInOption): CommandDirective[A] =
    self match {
      case BuiltIn(x)            => BuiltIn(f(x))
      case x @ UserDefined(_, _) => x
    }

}
object CommandDirective {
  final case class BuiltIn(option: BuiltInOption)                   extends CommandDirective[Nothing]
  final case class UserDefined[A](leftover: List[String], value: A) extends CommandDirective[A]

  def builtIn(option: BuiltInOption): CommandDirective[Nothing] = BuiltIn(option)

  def userDefined[A](leftover: List[String], value: A): CommandDirective[A] =
    UserDefined(leftover, value)
}
