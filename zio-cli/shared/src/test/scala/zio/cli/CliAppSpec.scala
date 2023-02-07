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

    def app(behavior: Task[Unit] = ZIO.unit) = CliApp.make(
      name = "Grumpy",
      version = "0.0.1",
      summary = HelpDoc.Span.text("this is some doc"),
      command = exampleCmd
    ) { case Subcommand.Make(_) => behavior }

    suite("exit code of the CLI")(
      test("should exit with 0 when the command is successful") {
        val result = app().run(List("make", "1"))

        assertZIO(result.exitCode)(equalTo(ExitCode.success))
      },
      test("should exit with a code <> 0 when the parsing of the command fails") {
        val result = app().run(List("make", "this is not an integer"))

        assertZIO(result.exitCode)(equalTo(ExitCode.failure))
      },
      test("should exit with a code <> 0 when the command fails") {
        val result = app(behavior = ZIO.fail(new RuntimeException("Boom"))).run(List("make", "1"))

        assertZIO(result.exitCode)(equalTo(ExitCode.failure))
      }
    )
  }

  override def spec: Spec[TestEnvironment & Scope, Any] =
    suite("CliApp")(
      exitCodeSpec
    )
}
