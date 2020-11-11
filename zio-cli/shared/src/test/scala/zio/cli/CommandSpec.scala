package zio.cli

import zio.test._
import zio.test.Assertion._

import scala.language.postfixOps

object CommandSpec extends DefaultRunnableSpec {
  def spec = suite("Command Spec")(
    suite("Toplevel Command Spec")(
      suite("Command with options followed by args")(
        testM("Should validate successfully") {
          assertM(Tail.command.validate(List("--n", "100", "foo.log"), ParserOptions.default))(
            equalTo((List.empty[String], BigInt(100), "foo.log"))
          )
        }
      )
    )
  )

  object Tail {
    val nFlag = Options.integer("n")

    val options: Options[BigInt]  = nFlag
    val args: Args.Single[String] = Args.text("file")

    val command = Command("tail")
      .options(options)
      .args(args)
  }

  object WC {
    val bytesFlag: Options[Boolean] = Options.bool("c", true)
    val linesFlag: Options[Boolean] = Options.bool("l", true)
    val wordsFlag: Options[Boolean] = Options.bool("w", true)
    val charFlag: Options[Boolean]  = Options.bool("m", false)

    val options = bytesFlag :: linesFlag :: wordsFlag :: charFlag

    val args = Args.text("files") *

    val command = Command("wc")
      .options(options)
      .args(args)
  }
}
