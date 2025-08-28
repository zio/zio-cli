package zio.cli

import zio.test._
import zio.test.Assertion._

object CliErrorSpec extends ZIOSpecDefault {
  def spec =
    suite("CliError")(
      suite("getCause")(
        test("BuiltIn") {
          val ex       = new IllegalArgumentException("built-in error")
          val cliError = CliError.BuiltIn(ex)
          assert(cliError.getCause)(equalTo(ex))
        },
        test("IO") {
          val ex       = new java.io.IOException("io error")
          val cliError = CliError.IO(ex)
          assert(cliError.getCause)(equalTo(ex))
        },
        test("Parsing") {
          val validationError = ValidationError(ValidationErrorType.InvalidValue, HelpDoc.empty)
          val cliError        = CliError.Parsing(validationError)
          assert(cliError.getCause)(equalTo(validationError))
        },
        suite("Execution")(
          test("Throwable") {
            val ex       = new RuntimeException("test error")
            val cliError = CliError.Execution(ex)

            assert(cliError.getCause)(equalTo(ex))
          },
          test("non-Throwable") {
            val cliError = CliError.Execution("BOOM!")
            assert(cliError.getCause)(isNull)
          }
        )
      )
    )

}
