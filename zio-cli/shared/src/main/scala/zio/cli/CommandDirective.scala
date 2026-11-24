package zio.cli

/**
 * What a parsed command line asks for: run the application with a value (`UserDefined`), or carry out a built-in option such as help (`BuiltIn`). `Command#parse` returns it.
 */
sealed trait CommandDirective[+A] { self =>
  import CommandDirective._

  /**
   * Transforms the value of a `UserDefined` directive; returns a `BuiltIn` directive unchanged.
   */
  def map[B](f: A => B): CommandDirective[B] =
    self match {
      case x @ BuiltIn(_)               => x
      case UserDefined(leftover, value) => CommandDirective.UserDefined(leftover, f(value))
    }

  /**
   * Transforms the option of a `BuiltIn` directive; returns a `UserDefined` directive unchanged.
   */
  def mapBuiltIn(f: BuiltInOption => BuiltInOption): CommandDirective[A] =
    self match {
      case BuiltIn(x)            => BuiltIn(f(x))
      case x @ UserDefined(_, _) => x
    }

}
/**
 * The cases of [[CommandDirective]] and their constructors.
 */
object CommandDirective {
  /**
   * Carry out the built-in `option`.
   */
  final case class BuiltIn(option: BuiltInOption)                   extends CommandDirective[Nothing]
  /**
   * Run the application with `value`. `leftover` holds the words that parsing didn't read.
   */
  final case class UserDefined[A](leftover: List[String], value: A) extends CommandDirective[A]

  /**
   * Creates a `BuiltIn` directive.
   */
  def builtIn(option: BuiltInOption): CommandDirective[Nothing] = BuiltIn(option)

  /**
   * Creates a `UserDefined` directive.
   */
  def userDefined[A](leftover: List[String], value: A): CommandDirective[A] =
    UserDefined(leftover, value)
}
