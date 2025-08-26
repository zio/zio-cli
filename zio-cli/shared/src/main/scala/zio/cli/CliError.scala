package zio.cli

import java.io.IOException
import zio.cli.CliError.BuiltIn
import zio.cli.CliError.IO
import zio.cli.CliError.Parsing
import zio.cli.CliError.Execution

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

object CliError {

  case class BuiltIn(e: Exception) extends CliError[Nothing]

  case class IO(e: IOException) extends CliError[Nothing]

  case class Parsing(e: ValidationError) extends CliError[Nothing]

  case class Execution[+E](e: E) extends CliError[E]

}
