package zio.cli

import zio.test._
import zio.test.Assertion._
import zio.cli.HelpDoc.{ p, Sequence }
import zio.cli.HelpDoc.Span.error

import scala.language.postfixOps

object CommandSpec extends DefaultRunnableSpec {
  def spec = suite("Command Spec")(
    suite("Toplevel Command Spec")(
      suite("Command with options followed by args")(
        testM("Should validate successfully") {
          assertM(Tail.command.parse(List("-n", "100", "foo.log"), ParserOptions.default))(
            equalTo((List.empty[String], (BigInt(100), "foo.log")))
          )
        },
        testM("Should provide auto correct suggestions for misspelled options") {
          assertM(Ag.command.parse(List("--afte", "2", "--before", "3", "fooBar"), ParserOptions.default).either)(
            equalTo(Left(p(error(s"""the flag "--afte" is not recognized. Did you mean --after?"""))))
          ) *>
          assertM(Ag.command.parse(List("--after", "2", "--efore", "3", "fooBar"), ParserOptions.default).either)(
            equalTo(Left(p(error(s"""the flag "--efore" is not recognized. Did you mean --before?"""))))
          ) *>
          assertM(Ag.command.parse(List("--afte", "2", "--efore", "3", "fooBar"), ParserOptions.default).either)(
            equalTo(Left(Sequence(
                p(error(s"""the flag "--afte" is not recognized. Did you mean --after?""")),
                p(error(s"""the flag "--efore" is not recognized. Did you mean --before?"""))
              )
            ))
          )
        },
        testM("Shows an error if an option is missing") {
          assertM(Ag.command.parse(List("--a", "2", "--before", "3", "fooBar"), ParserOptions.default).either)(
            equalTo(Left(p(error("Expected to find --after option."))))
          )
        }
      )
    )
  )

  object Tail {
    val nFlag = Options.integer("n")

    val options: Options[BigInt] = nFlag
    val args: Args[String]       = Args.text("file")

    val command = Command("tail", options, args)
  }

  object WC {
    val bytesFlag: Options[Boolean] = Options.bool("c", true)
    val linesFlag: Options[Boolean] = Options.bool("l", true)
    val wordsFlag: Options[Boolean] = Options.bool("w", true)
    val charFlag: Options[Boolean]  = Options.bool("m", false)

    val options = bytesFlag :: linesFlag :: wordsFlag :: charFlag

    val args = Args.text("files") *

    val command = Command("wc", options, args)
  }

  object Ag {
    val afterFlag: Options[BigInt] = Options.integer("after").alias("A")
    val beforeFlag: Options[BigInt] = Options.integer("before").alias("B")

    val options = afterFlag :: beforeFlag

    val args = Args.text

    val command = Command("grep", options, args)

  }
}
