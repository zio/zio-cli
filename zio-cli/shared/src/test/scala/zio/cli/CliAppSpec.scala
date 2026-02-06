package zio.cli

import zio._
import zio.test.Assertion._
import zio.test._

object CliAppSpec extends ZIOSpecDefault {

  private val exitCodeSpec = {
    sealed trait Subcommand extends Product with Serializable
    object Subcommand {
      final case class Make(int: BigInt) extends Subcommand
    }

    val makeCmd: Command[Subcommand] =
      Command(
        name = "make",
        args = Args.integer
      ).map(v => Subcommand.Make(v))

    val exampleCmd: Command[Subcommand] = Command("example").subcommands(makeCmd)

    def app(behavior: Task[Unit] = ZIO.unit): CliApp[Any, Throwable, Unit] = CliApp.make(
      name = "Grumpy",
      version = "0.0.1",
      summary = HelpDoc.Span.text("this is some doc"),
      command = exampleCmd
    ) { case Subcommand.Make(_) => behavior }

    suite("exit code of the CLI")(
      test("should exit with 0 when the command is successful") {
        val result = app().run(List("make", "1")).exit

        assertZIO(result)(succeeds(anything))
      },
      test("should exit with a code <> 0 when the parsing of the command fails") {
        val result = app().run(List("make", "this is not an integer")).exit

        assertZIO(result)(fails(anything))
      },
      test("should exit with a code <> 0 when the command fails") {
        val result = app(behavior = ZIO.fail(new RuntimeException("Boom"))).run(List("make", "1")).exit

        assertZIO(result)(fails(anything))
      }
    )
  }

  private val unrecognizedArgsSpec = {
    val cmd: Command[String] = Command("cli", Args.text("xyz"))

    def app(config: CliConfig = CliConfig.default): CliApp[Any, Nothing, String] = CliApp.make(
      name = "Test",
      version = "0.0.1",
      summary = HelpDoc.Span.text("test"),
      command = cmd,
      config = config
    )(input => ZIO.succeed(input))

    suite("unrecognized arguments")(
      test("should fail with non-zero exit code when extra arguments are provided") {
        val result = app().run(List("xyz", "abc")).exit

        assertZIO(result)(fails(anything))
      },
      test("should succeed when no extra arguments are provided") {
        val result = app().run(List("xyz")).exit

        assertZIO(result)(succeeds(anything))
      },
      test("should succeed with extra arguments when ignoreUnrecognized is true") {
        val config = CliConfig.default.copy(ignoreUnrecognized = true)
        val result = app(config).run(List("xyz", "abc")).exit

        assertZIO(result)(succeeds(anything))
      }
    )
  }

  override def spec: Spec[TestEnvironment & Scope, Any] =
    suite("CliApp")(
      exitCodeSpec,
      unrecognizedArgsSpec
    )
}
