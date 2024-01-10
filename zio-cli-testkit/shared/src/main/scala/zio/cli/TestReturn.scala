package zio.cli.testkit

import zio.cli.CommandDirective
import zio.cli.CommandDirective._

sealed trait TestReturn[+A]

object TestReturn {

  def convert[A](res: CommandDirective[A]): TestReturn[A] =
    res match {
      case UserDefined(_, value) => Value[A](value)
      case BuiltIn(_)            => Help()
    }

  sealed trait BuiltIn extends TestReturn[Nothing]

  case class Help()   extends BuiltIn
  case class Wizard() extends BuiltIn
  case class SH()     extends BuiltIn
  case class SHH()    extends BuiltIn

  case class Value[A](a: A) extends TestReturn[A]

}
