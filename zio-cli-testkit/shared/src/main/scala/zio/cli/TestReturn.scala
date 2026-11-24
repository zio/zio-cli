package zio.cli.testkit

import zio.cli.CommandDirective
import zio.cli.CommandDirective._

/**
 * What parsing a command line produced, in a form that is simple to compare in a test: either a `Value` that the
 * command parsed to, or one of the built-in options. [[TestReturn.convert]] builds it from a
 * `zio.cli.CommandDirective`, and `CliAssertion.assertCommandAll` hands it to the generated assertion.
 */
sealed trait TestReturn[+A]

/** The cases of [[TestReturn]] and the conversion from a `CommandDirective`. */
object TestReturn {

  /**
   * Converts the directive that parsing returned: a `UserDefined` directive becomes `Value` with its value, and any
   * `BuiltIn` directive becomes `Help()`. It never returns `Wizard`, `SH` or `SHH`, so `--wizard` also gives `Help()`.
   */
  def convert[A](res: CommandDirective[A]): TestReturn[A] =
    res match {
      case UserDefined(_, value) => Value[A](value)
      case BuiltIn(_)            => Help()
    }

  /**
   * A result for the built-in options of a command line, which produce no value.
   */
  sealed trait BuiltIn extends TestReturn[Nothing]

  /** The result [[TestReturn.convert]] gives for every built-in directive, including `--help`. */
  case class Help() extends BuiltIn

  /** A result that stands for the wizard option. [[TestReturn.convert]] never returns it. */
  case class Wizard() extends BuiltIn

  /** A built-in result named `SH`. [[TestReturn.convert]] never returns it. */
  case class SH() extends BuiltIn

  /** A built-in result named `SHH`. [[TestReturn.convert]] never returns it. */
  case class SHH() extends BuiltIn

  /** The result of a command line that parsed to the value `a`. */
  case class Value[A](a: A) extends TestReturn[A]

}
