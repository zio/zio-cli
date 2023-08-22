package zio.cli.testkit

import zio.cli._
import zio.test._
import zio.ZIO

/**
  * `GenerationAssertion` contains methods to test the a function returning `CliApp` or `Command`.
  */

object GenerationAssertion {

  implicit val cliConfig: CliConfig = CliConfig.default

  def commandCreation[A, B, R1, R2](
    f: A => Command[B], 
    response: Gen[R1, (A, Gen[R2, CliRepr[List[String], B]])]
    )(implicit cliConfig: CliConfig): ZIO[R1 with R2, Throwable, TestResult] =
    check(response) { case (seed, resp) =>
      CliAssertion.assertCommand[B, R2](f(seed), resp)
    }

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

  

  def cliAppCreation[R, E, A, B, RGen](f: A => CliApp[R, E, B], response: Gen[RGen, (A, Gen[RGen, CliRepr[List[String], B]])]): ZIO[R with RGen, Throwable, TestResult] =
    check(response) { case (seed, resp) =>
      CliAssertion.assertCliAppValues[R, E, B, RGen](f(seed), resp)
    }

  def cliAppCreationZIO[R, E, A, B, RGen, RC, EC <: Throwable](f: A => ZIO[RC, EC, CliApp[R, E, B]], response: Gen[RGen, (A, Gen[RGen, CliRepr[List[String], B]])]): ZIO[R with RC with RGen, Throwable, TestResult] =
    check(response) { case (seed, resp) =>
      for {
        cliApp <- f(seed)
        testRes <- CliAssertion.assertCliAppValues[R, E, B, RGen](cliApp, resp)
      } yield testRes     
    }



}
