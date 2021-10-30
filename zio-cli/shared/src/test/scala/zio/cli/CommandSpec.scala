package zio.cli

import zio.cli.BuiltInOption.ShowHelp
import zio.cli.HelpDoc.Span.error
import zio.cli.HelpDoc.{Sequence, p}
import zio.test.Assertion._
import zio.test._

import scala.language.postfixOps

object CommandSpec extends DefaultRunnableSpec {

  def spec = suite("Command Spec")(
    suite("Toplevel Command Spec")(
      suite("Command with options followed by args")(
        testM("Should validate successfully") {
          assertM(Tail.command.parse(List("tail", "-n", "100", "foo.log"), CliConfig.default))(
            equalTo(CommandDirective.UserDefined(List.empty[String], (BigInt(100), "foo.log")))
          ) *>
            assertM(Ag.command.parse(List("grep", "--after", "2", "--before", "3", "fooBar"), CliConfig.default))(
              equalTo(CommandDirective.UserDefined(List.empty[String], ((BigInt(2), BigInt(3)), "fooBar")))
            )
        },
        testM("Should provide auto correct suggestions for misspelled options") {
          assertM(
            Ag.command
              .parse(List("grep", "--afte", "2", "--before", "3", "fooBar"), CliConfig.default)
              .either
              .map(_.left.map(_.error))
          )(
            equalTo(Left(p(error("""The flag "--afte" is not recognized. Did you mean --after?"""))))
          ) *>
            assertM(
              Ag.command
                .parse(List("grep", "--after", "2", "--efore", "3", "fooBar"), CliConfig.default)
                .either
                .map(_.left.map(_.error))
            )(
              equalTo(Left(p(error("""The flag "--efore" is not recognized. Did you mean --before?"""))))
            ) *>
            assertM(
              Ag.command
                .parse(List("grep", "--afte", "2", "--efore", "3", "fooBar"), CliConfig.default)
                .either
                .map(_.left.map(_.error))
            )(
              equalTo(
                Left(
                  Sequence(
                    p(error("""The flag "--afte" is not recognized. Did you mean --after?""")),
                    p(error("""The flag "--efore" is not recognized. Did you mean --before?"""))
                  )
                )
              )
            )
        },
        testM("Shows an error if an option is missing") {
          assertM(
            Ag.command
              .parse(List("grep", "--a", "2", "--before", "3", "fooBar"), CliConfig.default)
              .either
              .map(_.left.map(_.error))
          )(
            equalTo(Left(p(error("Expected to find --after option."))))
          )
        }
      )
    ),
    suite("test commands joined by | operator")(
      testM("") {
        val orElseCommand =
          Command("remote", Options.Empty, Args.none) | Command("log", Options.Empty, Args.none)

        assertM(orElseCommand.parse(List("log"), CliConfig.default))(
          equalTo(CommandDirective.UserDefined(Nil, ((), ())))
        )
      }
    ),
    suite("test commands with clustered options")(
      testM("Clustered boolean options are equal to un-clustered options") {
        val clustered =
          WC.command
            .parse(List("wc", "-clw", "filename"), CliConfig.default)

        val unClustered =
          WC.command
            .parse(List("wc", "-c", "-l", "-w", "filename"), CliConfig.default)

        val commandDirective = CommandDirective.UserDefined(Nil, ((true, true, true, true), List("filename")))

        assertM(clustered)(equalTo(commandDirective))
        assertM(unClustered)(equalTo(commandDirective))
      }
    ),
    suite("SubCommand Suite")(
      suite("having two sub commands without options or arguments")({

        val git =
          Command("git", Options.Empty, Args.none).subcommands(
            Command("remote", Options.Empty, Args.none),
            Command("log", Options.Empty, Args.none)
          )

        Vector(
          testM("match first sub command without any surplus arguments") {
            assertM(git.parse(List("git", "remote"), CliConfig.default))(
              equalTo(CommandDirective.UserDefined(Nil, (((), ()), ((), ()))))
            )
          },
          testM("match first sub command with a surplus options") {
            assertM(git.parse(List("git", "remote", "-v"), CliConfig.default))(
              equalTo(CommandDirective.UserDefined(List("-v"), (((), ()), ((), ()))))
            )
          },
          testM("match second sub command without any surplus arguments") {
            assertM(git.parse(List("git", "log"), CliConfig.default))(
              equalTo(CommandDirective.UserDefined(Nil, (((), ()), ((), ()))))
            )
          }
        )
      }: _*),
      suite("sub command usage with options and arguments")({

        val git =
          Command("git", Options.Empty, Args.none).subcommands(
            Command("rebase", Options.boolean("i", true), Args.text ++ Args.text)
          )

        Vector(
          testM("test sub command with options and arguments") {
            assertM(git.parse(List("git", "rebase", "-i", "upstream", "branch"), CliConfig.default))(
              equalTo(CommandDirective.UserDefined(Nil, (((), ()), (true, ("upstream", "branch")))))
            )
          },
          testM("test unknown sub command") {
            assertM(git.parse(List("git", "abc"), CliConfig.default).flip.map(_.validationErrorType))(
              equalTo(ValidationErrorType.CommandMismatch)
            )
          },
          testM("test without sub command") {
            git.parse(List("git"), CliConfig.default).map { result =>
              assertTrue {
                result match {
                  case CommandDirective.BuiltIn(ShowHelp(_)) => true
                  case _                                     => false
                }
              }
            }
          }
        )
      }: _*),
      suite("test sub sub commands") {

        val command =
          Command("command", Options.Empty, Args.none).subcommands(
            Command("sub", Options.Empty, Args.none).subcommands(
              Command("subsub", Options.boolean("i", true), Args.text)
            )
          )

        testM("sub sub command with option and argument")(
          assertM(command.parse(List("command", "sub", "subsub", "-i", "text"), CliConfig.default))(
            equalTo(CommandDirective.UserDefined(Nil, (((), ()), (((), ()), (true, "text")))))
          )
        )
      }
    )
  )

  object Tail {
    val nFlag = Options.integer("n")

    val options: Options[BigInt] = nFlag
    val args: Args[String]       = Args.text("file")

    val command = Command("tail", options, args)
  }

  object WC {
    val bytesFlag: Options[Boolean] = Options.boolean("c", true)
    val linesFlag: Options[Boolean] = Options.boolean("l", true)
    val wordsFlag: Options[Boolean] = Options.boolean("w", true)
    val charFlag: Options[Boolean]  = Options.boolean("m", false)

    val options = bytesFlag ++ linesFlag ++ wordsFlag ++ charFlag

    val args = Args.text("files") *

    val command = Command("wc", options, args)
  }

  object Ag {
    val afterFlag: Options[BigInt]  = Options.integer("after").alias("A")
    val beforeFlag: Options[BigInt] = Options.integer("before").alias("B")

    val options = afterFlag ++ beforeFlag

    val args = Args.text

    val command = Command("grep", options, args)

  }
}
