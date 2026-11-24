package zio.cli.testkit

import zio.cli._
import zio.test._
import zio.ZIO

/**
 * Assertions for testing a function that builds a `zio.cli.Command` or a `zio.cli.CliApp` from a seed value. Each
 * method generates seeds, builds the command or app from each one, and checks it with the matching assertion of
 * [[CliAssertion]].
 *
 * {{{
 * import zio.cli._
 * import zio.test._
 *
 * object GreeterSpec extends ZIOSpecDefault {
 *   import GenerationAssertion._
 *
 *   def greeter(name: String): Command[String] =
 *     Command(name, Options.text("greeting")).map(greeting => greeting + " from " + name)
 *
 *   val seeds: Gen[Any, (String, Gen[Any, CliRepr[List[String], String]])] =
 *     Gen.fromIterable(List("alice", "bob")).map { name =>
 *       (name, Gen.const(CliRepr(List(name, "--greeting", "Hi"), "Hi from " + name)))
 *     }
 *
 *   def spec = suite("greeter")(
 *     test("every seed builds a working command") {
 *       commandCreation(greeter _, seeds)
 *     }
 *   )
 * }
 * }}}
 */
object GenerationAssertion {

  /**
   * The `zio.cli.CliConfig` (`CliConfig.default`) that the methods with an implicit `CliConfig` parameter pick up, such
   * as `commandCreation`. Import `GenerationAssertion._` (or `GenerationAssertion.cliConfig`) to have it in implicit
   * scope.
   */
  implicit val cliConfig: CliConfig = CliConfig.default

  /**
   * Tests a function that builds a `Command`. `response` is a generator of pairs `(seed, res)`: `seed` is passed to `f`
   * to build a `Command`, and `res` is a generator of `CliRepr[List[String], B]`, that is, pairs of a list of
   * command-line arguments and the value that the `Command` must parse them to. Each `Command` is checked against its
   * `res` with `CliAssertion.assertCommand`.
   */
  def commandCreation[A, B, R1, R2](
    f: A => Command[B],
    response: Gen[R1, (A, Gen[R2, CliRepr[List[String], B]])]
  )(implicit cliConfig: CliConfig): ZIO[R1 with R2, Throwable, TestResult] =
    check(response) { case (seed, resp) =>
      CliAssertion.assertCommand[B, R2](f(seed), resp)
    }

  /**
   * Like `commandCreation`, but `f` builds the `Command` in an effect. A failure of that effect fails the returned
   * effect.
   */
  def commandCreationZIO[A, B, R, E <: Throwable, RGen1, RGen2](
    f: A => ZIO[R, E, Command[B]],
    response: Gen[RGen1, (A, Gen[RGen2, CliRepr[List[String], B]])]
  ): ZIO[R with RGen1 with RGen2, Throwable, TestResult] =
    check(response) { case (seed, resp) =>
      for {
        command <- f(seed)
        testRes <- CliAssertion.assertCommand[B, RGen2](command, resp)
      } yield testRes
    }

  /**
   * Tests a function that builds a `CliApp`. `response` is a generator of pairs `(seed, res)`: `seed` is passed to `f`
   * to build a `CliApp`, and `res` is a generator of `CliRepr[List[String], B]`, that is, pairs of a list of
   * command-line arguments and the value that the app must return for them. Each `CliApp` is checked against its `res`
   * with `CliAssertion.assertCliAppValues`.
   */
  def cliAppCreation[R, E, A, B, RGen](
    f: A => CliApp[R, E, B],
    response: Gen[RGen, (A, Gen[RGen, CliRepr[List[String], B]])]
  ): ZIO[R with RGen, Throwable, TestResult] =
    check(response) { case (seed, resp) =>
      CliAssertion.assertCliAppValues[R, E, B, RGen](f(seed), resp)
    }

  /**
   * Like `cliAppCreation`, but `f` builds the `CliApp` in an effect. A failure of that effect fails the returned
   * effect.
   */
  def cliAppCreationZIO[R, E, A, B, RGen, RC, EC <: Throwable](
    f: A => ZIO[RC, EC, CliApp[R, E, B]],
    response: Gen[RGen, (A, Gen[RGen, CliRepr[List[String], B]])]
  ): ZIO[R with RC with RGen, Throwable, TestResult] =
    check(response) { case (seed, resp) =>
      for {
        cliApp  <- f(seed)
        testRes <- CliAssertion.assertCliAppValues[R, E, B, RGen](cliApp, resp)
      } yield testRes
    }

}
