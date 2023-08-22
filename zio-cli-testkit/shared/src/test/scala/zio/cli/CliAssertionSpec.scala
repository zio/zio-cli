package zio.cli.testkit

import zio.test._
import zio.cli._
import zio.cli.testkit.CliAssertion._
import zio.ZIO
import zio.Console._

object CliAssertionSpec extends ZIOSpecDefault {

  val synopsis1 = "command --opt1 text"
  val command1  = Command("command", Options.text("opt1"))

  val cliApp1 = CliApp.make("sample", "0", HelpDoc.Span.empty, command1)(x => ZIO.succeed(s"Options1: $x"))

  val spec = suite("CliAssertionSpec")(
    suite("Access methods")(
      test("accessCommand") {

        val assertion: Assertion[Command[_]] =
          Assertion(TestArrow.fromFunction { case command =>
            command.names.contains("command")
          })

        accessCommand(cliApp1, assertion)
      },
      test("accessHelpDoc") {
        val assertion: Assertion[HelpDoc] = Assertion.equalTo(Command("command", Options.text("opt1")).helpDoc)

        accessHelpDoc(cliApp1, assertion)
      },
      test("accessSynopsis") {
        val assertion: Assertion[UsageSynopsis] = Assertion(
          TestArrow.fromFunction[UsageSynopsis, Boolean](usageSynopsis =>
            usageSynopsis.enumerate(CliConfig.default).map(_.text) == List("command --opt1 <text>")
          )
        )

        accessSynopsis(cliApp1, assertion)
      }
    ),
    suite("Assert methods")(
      test("assertSynopsis") {
        assertSynopsis(cliApp1, synopsis1)
        assertSynopsis(cliApp1, synopsis1 :: Nil)
        assertSynopsis(
          cliApp1,
          UsageSynopsis.Named(List("command"), None) + UsageSynopsis.Named(List("--opt1"), Some("<text>"))
        )
        assertSynopsis(command1, synopsis1)
        assertSynopsis(command1, synopsis1 :: Nil)
        assertSynopsis(
          command1,
          UsageSynopsis.Named(List("command"), None) + UsageSynopsis.Named(List("--opt1"), Some("<text>"))
        )

      },
      test("assertHelpDoc") {
        assertHelpDoc(cliApp1, Command("command", Options.text("opt1")).helpDoc)
        assertHelpDoc(command1, Command("command", Options.text("opt1")).helpDoc)

      },
      test("assertCommand") {
        val gen: Gen[Any, CliRepr[List[String], String]] = Gen
          .fromIterable(
            List(
              (List("command", "--opt1", "a"), "a"),
              (List("command", "--opt1", "asd"), "asd")
            )
          )
          .map { case (x, y) =>
            CliRepr(x, y)
          }

        assertCommand(command1, gen)
        assertCommandAll[String, Any](
          command1,
          gen.map(
            _.map2 { case a: String =>
              Assertion.equalTo(Right(TestReturn.Value(a)))
            }
          )
        )
      },
      test("assertCliApp") {
        val gen1: Gen[Any, CliRepr[List[String], String]] = Gen
          .fromIterable(
            List(
              (List("--opt1", "a"), "Options1: a"),
              (List("--opt1", "asd"), "Options1: asd")
            )
          )
          .map { case (x, y) =>
            CliRepr(x, y)
          }

        val gen2: Gen[Any, CliRepr[List[String], ZIO[Any, Nothing, String]]] = gen1.map(_.map2(ZIO.succeed(_)))

        assertCliAppValues(cliApp1, gen1)
        assertCliAppZIO(cliApp1, gen2)
      }
    ),
    test("outputContains") {
      val newCli = cliApp1.flatMap(x => printLine("a") *> printLine("b") *> printLine(s"c $x"))

      val pairs: Gen[Any, CliRepr[List[String], List[String]]] =
        Gen.fromIterable(
          List(
            CliRepr(List("--opt1", "asd"), "asd" :: Nil),
            CliRepr(List("--opt1", "asd"), "b" :: Nil)
          )
        )

      outputContains(newCli, pairs)
    }
  )

}
