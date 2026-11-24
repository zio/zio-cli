package zio.cli.testkit

import zio.cli._
import zio.cli.CliApp.CliAppImpl
import zio.ZIO
import zio.test._

/**
 * Assertions for testing a `zio.cli.CliApp` or a `zio.cli.Command` inside a zio-test spec. The `assertSynopsis` and
 * `assertHelpDoc` methods compare the usage synopsis and the help text, the `assertCommand*` methods parse generated
 * command lines with a `Command`, and the `assertCliApp*` and `outputContains` methods run a `CliApp` on them. The
 * `assertCommand*`, `assertCliApp*` and `outputContains` methods take a `Gen` of [[CliRepr]] pairs, in which the
 * `value` is the list of command-line arguments and the `repr` is the expected result.
 *
 * {{{
 * import zio._
 * import zio.cli._
 * import zio.test._
 *
 * object GreetSpec extends ZIOSpecDefault {
 *   import CliAssertion._
 *
 *   val greet = Command("greet", Options.text("name"))
 *   val app   = CliApp.make("greeter", "1.0", HelpDoc.Span.empty, greet)(name => ZIO.succeed("Hello, " + name))
 *
 *   def spec = suite("greet")(
 *     test("synopsis") {
 *       assertSynopsis(greet, "greet --name <text>")
 *     },
 *     test("parsing") {
 *       assertCommand(greet, Gen.const(CliRepr(List("greet", "--name", "Ann"), "Ann")))
 *     },
 *     test("running the app") {
 *       assertCliAppValues(app, Gen.const(CliRepr(List("--name", "Ann"), "Hello, Ann")))
 *     }
 *   )
 * }
 * }}}
 */
object CliAssertion {

  /**
   * The `zio.cli.CliConfig` that the assertions here take as an implicit parameter, set to `CliConfig.default`. Import
   * `CliAssertion._` (or `CliAssertion.cliConfig`) to have it in implicit scope.
   */
  implicit val cliConfig: CliConfig = CliConfig.default

  /**
   * Runs `assert` on the `Command` that `cliApp` was built with and returns the result. Use it when none of the other
   * methods covers the property to check.
   */
  def accessCommand[R, E, A](cliApp: CliApp[R, E, A], assert: Assertion[Command[_]]): TestResult =
    cliApp match {
      case CliAppImpl(_, _, _, command, _, _, _, _) => assert.run(command)
    }

  /**
   * Runs `assert` on the `HelpDoc` of the `Command` that `cliApp` was built with (`command.helpDoc`) and returns the
   * result.
   */
  def accessHelpDoc[R, E, A](cliApp: CliApp[R, E, A], assert: Assertion[HelpDoc]): TestResult =
    cliApp match {
      case CliAppImpl(_, _, _, command, _, _, _, _) => assert.run(command.helpDoc)
    }

  /**
   * Runs `assert` on the `UsageSynopsis` of the `Command` that `cliApp` was built with (`command.synopsis`) and returns
   * the result.
   */
  def accessSynopsis[R, E, A](cliApp: CliApp[R, E, A], assert: Assertion[UsageSynopsis]): TestResult =
    cliApp match {
      case CliAppImpl(_, _, _, command, _, _, _, _) => assert.run(command.synopsis)
    }

  /**
   * Methods to test properties of the `Command` of a `CliApp`.
   */

  /**
   * Checks that the usage synopsis of `command`, enumerated with the implicit `cliConfig`, is the single line
   * `synopsis`. A synopsis that enumerates to several lines does not match this overload; use the `List[String]` one.
   */
  def assertSynopsis[A](command: Command[A], synopsis: String)(implicit cliConfig: CliConfig): TestResult =
    assertSynopsis(command, List(synopsis))

  /**
   * Checks that the usage synopsis of `command`, enumerated with the implicit `cliConfig`, is exactly the lines in
   * `synopsis`, in order.
   */
  def assertSynopsis[A](command: Command[A], synopsis: List[String])(implicit cliConfig: CliConfig): TestResult =
    assertTrue(command.synopsis.enumerate(cliConfig).map(_.text) == synopsis)

  /**
   * Checks that the usage synopsis of `command` has the same lines as `synopsis`, both enumerated with
   * `CliConfig.default`.
   */
  def assertSynopsis[A](command: Command[A], synopsis: UsageSynopsis): TestResult =
    assertSynopsis(command, synopsis.enumerate(CliConfig.default).map(_.text))(CliConfig.default)

  /**
   * Checks that the help text of `command` is the same as `helpDoc`. The two are compared through their `toHTML`
   * rendering.
   */
  def assertHelpDoc[A](command: Command[A], helpDoc: HelpDoc): TestResult =
    assertTrue(command.helpDoc.toHTML == helpDoc.toHTML)

  /**
   * Checks that the usage synopsis of the `Command` of `cliApp`, enumerated with the implicit `cliConfig`, is the
   * single line `synopsis`.
   */
  def assertSynopsis[R, E, A](cliApp: CliApp[R, E, A], synopsis: String)(implicit cliConfig: CliConfig): TestResult =
    assertSynopsis(cliApp, List(synopsis))

  /**
   * Checks that the usage synopsis of the `Command` of `cliApp`, enumerated with the implicit `cliConfig`, is exactly
   * the lines in `synopsis`, in order.
   */
  def assertSynopsis[R, E, A](cliApp: CliApp[R, E, A], synopsis: List[String])(implicit
    cliConfig: CliConfig
  ): TestResult =
    cliApp match {
      case CliAppImpl(_, _, _, command, _, _, _, _) => assertSynopsis(command, synopsis)
    }

  /**
   * Checks that the usage synopsis of the `Command` of `cliApp` has the same lines as `synopsis`, both enumerated with
   * `CliConfig.default`.
   */
  def assertSynopsis[R, E, A](cliApp: CliApp[R, E, A], synopsis: UsageSynopsis): TestResult =
    assertSynopsis(cliApp, synopsis.enumerate(CliConfig.default).map(_.text))

  /**
   * Checks that the help text of the `Command` of `cliApp` is the same as `helpDoc`, compared through their `toHTML`
   * rendering.
   */
  def assertHelpDoc[R, E, A](cliApp: CliApp[R, E, A], helpDoc: HelpDoc): TestResult =
    cliApp match {
      case CliAppImpl(_, _, _, command, _, _, _, _) => assertHelpDoc(command, helpDoc)
    }

  /**
   * Methods to assist property-based testing using `Gen` of a `Command`.
   */

  /**
   * Parses each generated argument list with `command` and checks the outcome with the generated `Assertion`. The
   * assertion sees `Right(value)` when parsing yields a value, and `Left(error)` when parsing fails with a
   * `zio.cli.ValidationError`. When parsing yields a built-in directive such as `--help` instead of a value, the
   * returned effect fails with a `java.lang.Exception` that has no message.
   */
  def assertCommandSuccess[A, R](
    command: Command[A],
    pairs: Gen[R, CliRepr[List[String], Assertion[Either[ValidationError, A]]]]
  )(implicit cliConfig: CliConfig): ZIO[R, Throwable, TestResult] =
    check(pairs) { case CliRepr(params, assertion) =>
      command
        .parse(params, cliConfig)
        .map(Right(_))
        .catchAll { case e: ValidationError =>
          ZIO.succeed(Left(e))
        }
        .flatMap {
          case Right(CommandDirective.UserDefined(_, value)) => ZIO.succeed(Right(value))
          case Right(_)                                      => ZIO.fail(new Exception)
          case Left(e)                                       => ZIO.succeed(Left(e))
        }
        .map(assertion.run(_))
    }

  /**
   * Parses each generated argument list with `command`, converts the outcome with [[TestReturn.convert]], and checks it
   * with the generated `Assertion`. The assertion sees `Right(TestReturn.Value(value))` when parsing yields a value,
   * `Right(TestReturn.Help())` when it yields a built-in directive (the `--wizard` option included), and `Left(error)`
   * when parsing fails with a `zio.cli.ValidationError`.
   */
  def assertCommandAll[A, R](
    command: Command[A],
    pairs: Gen[R, CliRepr[List[String], Assertion[Either[ValidationError, TestReturn[A]]]]]
  )(implicit cliConfig: CliConfig): ZIO[R, Throwable, TestResult] =
    check(pairs) { case CliRepr(params, assertion) =>
      command
        .parse(params, cliConfig)
        .map(TestReturn.convert)
        .map(Right(_))
        .catchSome { case e: ValidationError =>
          ZIO.succeed(Left(e))
        }
        .map(assertion.run(_))
    }

  /**
   * Parses each generated argument list with `command` and checks that the result is `Right(expected)`, where
   * `expected` is the `repr` of the pair. The returned `TestResult` is a failure when parsing fails with a
   * `zio.cli.ValidationError` or yields a different value. When parsing yields a built-in directive such as `--help`,
   * the returned effect fails with a `java.lang.Exception` that has no message.
   */
  def assertCommand[A, R](
    command: Command[A],
    pairs: Gen[R, CliRepr[List[String], A]]
  )(implicit cliConfig: CliConfig): ZIO[R, Throwable, TestResult] = {

    val assertion: A => Assertion[Either[ValidationError, A]] = a => Assertion.assertion("check")(_ == Right(a))

    assertCommandSuccess[A, R](
      command,
      pairs.map(_.map2(assertion))
    )
  }

  /**
   * Methods to test a `CliApp` instance.
   */

  /**
   * Runs `cliApp` on each generated argument list and checks the outcome with the generated `Assertion`. The assertion
   * sees `Right(value)` with the value the app returns, and `Left(error)` when the run fails with a `zio.cli.CliError`.
   * When the run returns no value, as it does for `--help`, the returned effect fails with a `java.lang.Exception` that
   * has no message. The argument list does not include the name of the command.
   */
  def assertCliApp[R, E, A, RGen](
    cliApp: CliApp[R, E, A],
    pairs: Gen[RGen, CliRepr[List[String], Assertion[Either[CliError[E], A]]]]
  )(implicit cliConfig: CliConfig): ZIO[RGen with R, Throwable, TestResult] =
    check(pairs) { case CliRepr(params, assertion) =>
      cliApp
        .run(params)
        .map(Right(_))
        .catchAll { case e: CliError[E] =>
          ZIO.succeed(Left(e))
        }
        .flatMap {
          case Right(Some(value)) => ZIO.succeed(Right(value))
          case Right(None)        => ZIO.fail(new Exception)
          case Left(e)            => ZIO.succeed(Left(e))
        }
        .map(assertion.run(_))
    }

  /**
   * Runs `cliApp` on each generated argument list and checks that the value it returns equals the `repr` of the pair.
   * This compares the returned value, not what the app prints; use `outputContains` for that. A run that fails with a
   * `zio.cli.CliError` makes the `TestResult` a failure.
   */
  def assertCliAppValues[R, E, A, RGen](
    cliApp: CliApp[R, E, A],
    pairs: Gen[RGen, CliRepr[List[String], A]]
  )(implicit cliConfig: CliConfig): ZIO[RGen with R, Throwable, TestResult] =
    assertCliApp[R, E, A, RGen](
      cliApp,
      pairs.map(
        _.map2 { case a =>
          Assertion.equalTo(Right(a))
        }
      )
    )

  /**
   * Runs `cliApp` on each generated argument list, runs the generated effect, and checks that both give the same
   * result. An error of the effect is compared as a `zio.cli.CliError.Execution` holding that error. A run of `cliApp`
   * that fails with any other `CliError`, such as a parsing error, makes the returned effect fail with it instead of
   * producing a `TestResult`.
   */
  def assertCliAppZIO[R, E, A, RGen](
    cliApp: CliApp[R, E, A],
    pairs: Gen[RGen, CliRepr[List[String], ZIO[R, E, A]]]
  ): ZIO[RGen with R, Throwable, TestResult] =
    check(pairs) { case CliRepr(params, effect) =>
      for {
        cliRes <- cliApp
                    .run(params)
                    .map(Right(_))
                    .catchAll {
                      case e: CliError.Execution[E] => ZIO.succeed(Left(e))
                      case e                        => ZIO.fail(e)
                    }
                    .flatMap {
                      case Right(Some(value)) => ZIO.succeed(Right(value))
                      case Right(None)        => ZIO.fail(new Exception)
                      case Left(e)            => ZIO.succeed(Left(e))
                    }
        realRes <- effect
                     .map(Right(_))
                     .catchAll { case e =>
                       ZIO.succeed(Left(CliError.Execution(e)))
                     }
      } yield assertTrue(realRes == cliRes)

    }

  /**
   * Methods for Output and Input testing
   */

  /**
   * Runs `cliApp` on each generated argument list and checks that each string of the `repr` is contained in at least
   * one entry of `TestConsole.output`, so it has to run inside a zio-test spec. A string that is split across two
   * entries does not match, and an empty `Seq` always passes. A run that fails makes the returned effect fail with that
   * error.
   */
  def outputContains[R, E, A, RGen](
    cliApp: CliApp[R, E, A],
    pairs: Gen[RGen, CliRepr[List[String], Seq[String]]]
  ): ZIO[RGen with R, Throwable, TestResult] =
    check(pairs) { case CliRepr(params, strings) =>
      for {
        _    <- cliApp.run(params)
        text <- zio.test.TestConsole.output
      } yield assertTrue(strings.forall(word => text.map(_.contains(word)).contains(true)))
    }

}
