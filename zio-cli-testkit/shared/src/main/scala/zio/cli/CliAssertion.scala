package zio.cli.testkit

import zio.cli._
import zio.cli.CliApp.CliAppImpl
import zio.ZIO
import zio.test._

/**
 * `CliAssertions` contains methods to test a particular `CliApp` or a particular `Command`. It's possible to test
 * directly a `Command` instance
 */

object CliAssertion {

  implicit val cliConfig: CliConfig = CliConfig.default

  /**
   * Allows to check any assertion on the `Command` instance employed in a `CliApp`. Better use only when other methods
   * do not cover your use-case.
   */
  def accessCommand[R, E, A](cliApp: CliApp[R, E, A], assert: Assertion[Command[_]]): TestResult =
    cliApp match {
      case CliAppImpl(_, _, _, command, _, _, _, _) => assert.run(command)
    }

  def accessHelpDoc[R, E, A](cliApp: CliApp[R, E, A], assert: Assertion[HelpDoc]): TestResult =
    cliApp match {
      case CliAppImpl(_, _, _, command, _, _, _, _) => assert.run(command.helpDoc)
    }

  def accessSynopsis[R, E, A](cliApp: CliApp[R, E, A], assert: Assertion[UsageSynopsis]): TestResult =
    cliApp match {
      case CliAppImpl(_, _, _, command, _, _, _, _) => assert.run(command.synopsis)
    }

  /**
   * Methods to test properties of the `Command` of a `CliApp`.
   */

  /**
   * Checks the usage synopsis in `String` or `UsageSynopsis` form.
   */
  def assertSynopsis[A](command: Command[A], synopsis: String)(implicit cliConfig: CliConfig): TestResult =
    assertSynopsis(command, List(synopsis))

  def assertSynopsis[A](command: Command[A], synopsis: List[String])(implicit cliConfig: CliConfig): TestResult =
    assertTrue(command.synopsis.enumerate(cliConfig).map(_.text) == synopsis)

  def assertSynopsis[A](command: Command[A], synopsis: UsageSynopsis): TestResult =
    assertSynopsis(command, synopsis.enumerate(CliConfig.default).map(_.text))(CliConfig.default)

  def assertHelpDoc[A](command: Command[A], helpDoc: HelpDoc): TestResult =
    assertTrue(command.helpDoc.toHTML == helpDoc.toHTML)

  /**
   * Checks the usage synopsis in `String` or `UsageSynopsis` form.
   */
  def assertSynopsis[R, E, A](cliApp: CliApp[R, E, A], synopsis: String)(implicit cliConfig: CliConfig): TestResult =
    assertSynopsis(cliApp, List(synopsis))

  def assertSynopsis[R, E, A](cliApp: CliApp[R, E, A], synopsis: List[String])(implicit
    cliConfig: CliConfig
  ): TestResult =
    cliApp match {
      case CliAppImpl(_, _, _, command, _, _, _, _) => assertSynopsis(command, synopsis)
    }

  def assertSynopsis[R, E, A](cliApp: CliApp[R, E, A], synopsis: UsageSynopsis): TestResult =
    assertSynopsis(cliApp, synopsis.enumerate(CliConfig.default).map(_.text))

  def assertHelpDoc[R, E, A](cliApp: CliApp[R, E, A], helpDoc: HelpDoc): TestResult =
    cliApp match {
      case CliAppImpl(_, _, _, command, _, _, _, _) => assertHelpDoc(command, helpDoc)
    }

  /**
   * Methods to assist property-based testing using `Gen` of a `Command`.
   */

  /**
   * Creates a TestResult from an Assertion over the result of parsing a command. It can test `ValidationError` produced
   * during the parsing process and the value produced by the `Command` instance.
   */
  def assertCommandSuccess[A, R](
    command: Command[A],
    pairs: Gen[R, CliRepr[List[String], Assertion[Either[ValidationError, A]]]]
  )(implicit cliConfig: CliConfig): ZIO[R, Throwable, TestResult] =
    check(pairs) { case CliRepr(params, assertion) =>
      command
        .parse(params, cliConfig, Nil)
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
   * Creates a TestResult from an Assertion over the result of parsing a command. It can test `ValidationError` produced
   * during the parsing process, the value produced by the `Command` instance and the triggering of built-in options.
   */
  def assertCommandAll[A, R](
    command: Command[A],
    pairs: Gen[R, CliRepr[List[String], Assertion[Either[ValidationError, TestReturn[A]]]]]
  )(implicit cliConfig: CliConfig): ZIO[R, Throwable, TestResult] =
    check(pairs) { case CliRepr(params, assertion) =>
      command
        .parse(params, cliConfig, Nil)
        .map(TestReturn.convert)
        .map(Right(_))
        .catchSome { case e: ValidationError =>
          ZIO.succeed(Left(e))
        }
        .map(assertion.run(_))
    }

  /**
   * Tests the value produced by the `Command` instance. It fails when there is a `ValidationError`.
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
   * Creates a TestResult from an Assertion over the result of running a command on a `CliApp`. It can test errors and
   * values produced by the `CliApp`.
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
   * Creates a TestResult to test the output of a `CliApp`.
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
   * Creates a TestResult to test the output of a `CliApp` using a ZIO effect.
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
   * Checks if the console output of a CliApp contains all the `String` from a `Seq[String]`.
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
