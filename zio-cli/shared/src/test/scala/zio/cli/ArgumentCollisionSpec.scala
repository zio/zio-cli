package zio.cli

import zio._
import zio.test.{TestAspect, TestConsole, ZIOSpecDefault, assertTrue}

object ArgumentCollisionSpec extends ZIOSpecDefault {

  def spec = suite("ArgumentCollisionSpec")(
    test("argument collision") {
      val a = Options.text("a").map(identity)
      val b = Options.text("b").map(identity)

      val argumentCollisionApp: CliApp[Any, Nothing, String] =
        CliApp.make(
          "test",
          "0.1.0",
          HelpDoc.Span.text("Reproduce issue"),
          command = Command("test", a | b)
        )(_ => ZIO.unit)

      for {
        _      <- argumentCollisionApp.run(List("-a", "a", "-b", "b")).either
        output <- TestConsole.output
      } yield assertTrue(output.head.contains("Options collision detected. You can only specify either -a or -b."))
    }
  ) @@ TestAspect.silent
}
