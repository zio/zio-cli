package zio.cli

import java.io.IOException

sealed trait CliError[+E] extends Exception

object CliError {

  case class BuiltIn(e: Exception) extends CliError[Nothing]

  case class IO(e: IOException) extends CliError[Nothing]

  case class Parsing(e: ValidationError) extends CliError[Nothing]

  case class Execution[+E](e: E) extends CliError[E]

}
