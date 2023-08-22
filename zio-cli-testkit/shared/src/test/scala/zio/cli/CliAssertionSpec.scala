package zio.cli.testkit

import zio.test._
import zio.cli._
import zio.cli.testkit.CliAssertion._
import zio.ZIO

object CliAssertionSpec extends ZIOSpecDefault {

    val synopsis1 = "command --opt1 text"
    val command1 = Command("command", Options.text("opt1"))

    val cliApp1 = CliApp.make("sample", "0",HelpDoc.Span.empty, command1)(x => ZIO.succeed(s"Options1: $x"))

    val spec = suite("CliAssertions")(
        suite("Access methods")(
            test("accessCommand"){

                val assertion: Assertion[Command[_]] = Assertion.equalTo(Command("command", Options.text("opt1")))

                accessCommand(cliApp1, assertion)
            },
            test("accessHelpDoc"){
                val assertion: Assertion[HelpDoc] = Assertion.equalTo(HelpDoc.empty)

                accessHelpDoc(cliApp1, assertion)
            },
            test("accessSynopsis"){
                val assertion: Assertion[UsageSynopsis] = Assertion(
                    TestArrow.fromFunction[UsageSynopsis, Boolean](usageSynopsis => usageSynopsis.enumerate(CliConfig.default) == List("command --opt1 text"))
                )

                accessSynopsis(cliApp1, assertion)
            },
        ),
        suite("Assert methods")(
            test("assertSynopsis"){
                assertSynopsis(cliApp1, synopsis1)
                assertSynopsis(cliApp1, synopsis1 :: Nil)
                assertSynopsis(cliApp1, UsageSynopsis.Named(List("command"), None) + UsageSynopsis.Named(List("opt1"), Some("text")))
                assertSynopsis(command1, synopsis1)
                assertSynopsis(command1, synopsis1 :: Nil)
                assertSynopsis(command1, UsageSynopsis.Named(List("command"), None) + UsageSynopsis.Named(List("opt1"), Some("text")))

            },
            test("assertHelpDoc"){
                assertHelpDoc(cliApp1, HelpDoc.empty)
                assertHelpDoc(command1, HelpDoc.empty)

            },
            test("assertCommand"){
                val gen: Gen[Any, CliRepr[List[String], String]] = Gen.fromIterable(
                    List((List("command", "--opt1", "a"), "a"), (List("command", "--opt1", "asd"), "asd"), (List("command", "--opt1", ""), ""))
                    ).map {
                    case (x, y) => CliRepr(x, y)
                }

                assertCommand(command1, gen)
                assertCommandAll[String, Any](
                    command1, 
                    gen.map(
                        _.map2{
                            case a: String => Assertion.equalTo(Right(TestReturn.Value(a)))
                        }
                    )
                )
            },
            test("assertCliApp"){
                val gen1: Gen[Any, CliRepr[List[String], String]] = Gen.fromIterable(
                    List((List("command", "--opt1", "a"), "a"), (List("command", "--opt1", "asd"), "asd"), (List("command", "--opt1", ""), ""))
                    ).map {
                    case (x, y) => CliRepr(x, y)
                }

                val gen2: Gen[Any, CliRepr[List[String], Assertion[String]]] = Gen.fromIterable(
                    List((List("command", "--opt1", "a"), Assertion.equalTo("a")), (List("command", "--opt1", "asd"), Assertion.equalTo("asd")), (List("command", "--opt1", ""), Assertion.equalTo("")))
                    ).map {
                    case (x, y) => CliRepr(x, y)
                }

                val gen3: Gen[Any, CliRepr[List[String], ZIO[Any, Nothing, String]]] = gen1.map(_.map2(ZIO.succeed(_)))

                assertCliAppValues(cliApp1, gen1)
                assertCliApp(cliApp1, gen2)
                assertCliAppZIO(cliApp1, gen3)
            }
        )

    )

}
