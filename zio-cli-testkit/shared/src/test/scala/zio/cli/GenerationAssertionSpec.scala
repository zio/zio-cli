package zio.cli.testkit

import zio.test._
import zio.cli._
import zio.cli.testkit.GenerationAssertion._
import zio.ZIO

object GenerationAssertionSpec extends ZIOSpecDefault {

  val spec = suite("GenerationAssertionSpec")(
    test("commandCreation") {

      def createCommand(header: String): Command[String] =
        Command(header, Options.text("opt1")).map(x => s"$header: $x")

      def createGen(header: String): Gen[Any, CliRepr[List[String], String]] =
        Gen.fromIterable(List("a", "ba", "")).map { case param =>
          CliRepr(
            List(header, "--opt1", param),
            s"$header: $param"
          )
        }

      val gen: Gen[Any, (String, Gen[Any, CliRepr[List[String], String]])] =
        Gen.fromIterable(List("header1", "header2")).map { case header =>
          (header, createGen(header))
        }

      commandCreation(createCommand _, gen)
      commandCreationZIO((x: String) => ZIO.succeed(createCommand(x)), gen)
    },
    test("cliAppCreation") {

      def createCliApp(header: String): CliApp[Any, Nothing, String] = CliApp.make(
        "name",
        "version",
        HelpDoc.Span.empty,
        Command(header, Options.text("opt1")).map(x => s"$header: $x")
      ) { case x =>
        ZIO.succeed(x)
      }

      def createGen(header: String): Gen[Any, CliRepr[List[String], String]] =
        Gen.fromIterable(List("a", "ba", "")).map { case param =>
          CliRepr(
            List("--opt1", param),
            s"$header: $param"
          )
        }

      val gen: Gen[Any, (String, Gen[Any, CliRepr[List[String], String]])] =
        Gen.fromIterable(List("header1", "header2")).map { case header =>
          (header, createGen(header))
        }

      cliAppCreation(createCliApp _, gen)
      cliAppCreation(createCliApp _, gen)

    }
  )

}
