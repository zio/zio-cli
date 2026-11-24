package zio.cli

import java.io.IOException
import zio.cli.CliError.BuiltIn
import zio.cli.CliError.IO
import zio.cli.CliError.Parsing
import zio.cli.CliError.Execution

/**
 * The error of running a [[CliApp]]: a parse error, a failure of a built-in option, a console error, or a failure `E`
 * of the application's function. `getMessage` always returns `zio-cli app failed`, and `getCause` returns the wrapped
 * error.
 */
sealed trait CliError[+E] extends Exception { self =>
  override final def getMessage(): String =
    "zio-cli app failed"
  override final def getCause(): Throwable =
    self match {
      case BuiltIn(e)              => e
      case IO(e)                   => e
      case Parsing(e)              => e
      case Execution(e: Throwable) => e
      case Execution(_)            => null
    }
}

/**
 * The cases of [[CliError]].
 */
object CliError {

  /**
   * A failure while running a built-in option, such as shell completion.
   */
  case class BuiltIn(e: Exception) extends CliError[Nothing]

  /**
   * A failure to write to the console.
   */
  case class IO(e: IOException) extends CliError[Nothing]

  /**
   * The command line didn't parse. `e` holds the error, which `CliApp#run` has already printed.
   */
  case class Parsing(e: ValidationError) extends CliError[Nothing]

  /**
   * The application's function failed with `e`. `getCause` returns `e` when it is a `Throwable`, and `null` otherwise.
   */
  case class Execution[+E](e: E) extends CliError[E]

}
