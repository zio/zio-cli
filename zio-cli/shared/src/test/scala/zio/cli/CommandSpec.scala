package zio.cli

import zio.cli.BuiltInOption.ShowHelp
import zio.cli.HelpDoc.Span.error
import zio.cli.HelpDoc.{Sequence, p}
import zio.test.Assertion._
import zio.test._

import scala.language.postfixOps
import zio.test.ZIOSpecDefault

object CommandSpec extends ZIOSpecDefault {

  def spec = suite("Command Spec")(
    suite("Toplevel Command Spec")(
      suite("Command with options followed by args")(
        test("Should validate successfully") {
          assertZIO(Tail.command.parse(List("tail", "-n", "100", "foo.log"), CliConfig.default))(
            equalTo(CommandDirective.UserDefined(List.empty[String], (BigInt(100), "foo.log")))
          ) *>
            assertZIO(Ag.command.parse(List("grep", "--after", "2", "--before", "3", "fooBar"), CliConfig.default))(
              equalTo(CommandDirective.UserDefined(List.empty[String], ((BigInt(2), BigInt(3)), "fooBar")))
            )
        },
        test("Should provide auto correct suggestions for misspelled options") {
          assertZIO(
            Ag.command
              .parse(List("grep", "--afte", "2", "--before", "3", "fooBar"), CliConfig.default)
              .either
              .map(_.left.map(_.error))
          )(
            equalTo(Left(p(error("""The flag "--afte" is not recognized. Did you mean --after?"""))))
          ) *>
            assertZIO(
              Ag.command
                .parse(List("grep", "--after", "2", "--efore", "3", "fooBar"), CliConfig.default)
                .either
                .map(_.left.map(_.error))
            )(
              equalTo(Left(p(error("""The flag "--efore" is not recognized. Did you mean --before?"""))))
            ) *>
            assertZIO(
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
        test("Shows an error if an option is missing") {
          assertZIO(
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
      test("") {
        val orElseCommand =
          Command("remote", Options.Empty, Args.none) | Command("log", Options.Empty, Args.none)

        assertZIO(orElseCommand.parse(List("log"), CliConfig.default))(
          equalTo(CommandDirective.UserDefined(Nil, ()))
        )
      }
    ),
    suite("test commands with clustered options")(
      test("Clustered boolean options are equal to un-clustered options") {
        val clustered =
          WC.command
            .parse(List("wc", "-clw", "filename"), CliConfig.default)

        val unClustered =
          WC.command
            .parse(List("wc", "-c", "-l", "-w", "filename"), CliConfig.default)

        val commandDirective = CommandDirective.UserDefined(Nil, ((true, true, true, true), List("filename")))

        assertZIO(clustered)(equalTo(commandDirective))
        assertZIO(unClustered)(equalTo(commandDirective))
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
          test("match first sub command without any surplus arguments") {
            assertZIO(git.parse(List("git", "remote"), CliConfig.default))(
              equalTo(CommandDirective.UserDefined(Nil, ()))
            )
          },
          test("match first sub command with a surplus options") {
            assertZIO(git.parse(List("git", "remote", "-v"), CliConfig.default))(
              equalTo(CommandDirective.UserDefined(List("-v"), ()))
            )
          },
          test("match second sub command without any surplus arguments") {
            assertZIO(git.parse(List("git", "log"), CliConfig.default))(
              equalTo(CommandDirective.UserDefined(Nil, ()))
            )
          }
        )
      }: _*),
      suite("sub command usage with options and arguments")({

        val git =
          Command("git", Options.Empty, Args.none).subcommands(
            Command(
              "rebase",
              Options.boolean("i", true) ++ Options.text("empty").withDefault("drop"),
              Args.text ++ Args.text
            )
          )

        Vector(
          test("test sub command with required options and arguments") {
            assertZIO(git.parse(List("git", "rebase", "-i", "upstream", "branch"), CliConfig.default))(
              equalTo(CommandDirective.UserDefined(Nil, ((true, "drop"), ("upstream", "branch"))))
            )
          },
          test("test sub command with required and optional options and arguments") {
            assertZIO(
              git.parse(List("git", "rebase", "-i", "--empty", "ask", "upstream", "branch"), CliConfig.default)
            )(
              equalTo(CommandDirective.UserDefined(Nil, ((true, "ask"), ("upstream", "branch"))))
            )
          },
          test("test unknown sub command") {
            assertZIO(git.parse(List("git", "abc"), CliConfig.default).flip.map(_.validationErrorType))(
              equalTo(ValidationErrorType.CommandMismatch)
            )
          },
          test("test without sub command") {
            git.parse(List("git"), CliConfig.default).map { result =>
              assertTrue {
                result match {
                  case CommandDirective.BuiltIn(ShowHelp(_, _)) => true
                  case _                                        => false
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

        test("sub sub command with option and argument")(
          assertZIO(command.parse(List("command", "sub", "subsub", "-i", "text"), CliConfig.default))(
            equalTo(CommandDirective.UserDefined(Nil, (true, "text")))
          )
        )
      }
    ),
    suite("Helpdoc On Command Suite")(
      suite("test adding helpdoc to commands")(
        test("add text helpdoc to Single") {
          val command = Command("tldr").withHelp("this is some help")
          assertZIO(command.parse(List("tldr"), CliConfig.default))(
            equalTo(CommandDirective.UserDefined(Nil, ()))
          )
        },
        test("helpdoc is on command") {
          val command = Command("tldr").withHelp("this is some help")
          val header  = HelpDoc.h1("description")
          assert(command.helpDoc)(
            equalTo(HelpDoc.Sequence(header, HelpDoc.p("this is some help")))
          )
        }
      ),
      suite("test adding helpdoc to sub commands") {
        val command =
          Command("command").subcommands(
            Command("sub").withHelp("this is some help")
          )
        val header = HelpDoc.h1("subcommands")
        test("helpdoc is on subcommand") {
          assert(command.helpDoc)(
            not(equalTo(HelpDoc.Sequence(header, HelpDoc.p("this is some help"))))
          )
        }
      },
      suite("test adding helpdoc to OrElse command") {
        val command = Command
          .OrElse(
            Command("command1").withHelp("this is some help with command1"),
            Command("command2")
          )

        test("helpdoc on orElse command")(
          assert(command.left.helpDoc)(
            equalTo(HelpDoc.Sequence(HelpDoc.h1("description"), HelpDoc.p("this is some help with command1")))
          )
        )
      },
      suite("test adding helpdoc to Map command") {
        val command = Command
          .Map[(String, String), Int](
            Command("command", Options.text("word"), Args.text).withHelp("this is some help with command"),
            _._2.length
          )

        test("helpdoc on Map command")(
          assert(command.helpDoc.toPlaintext())(
            containsString("this is some help with command")
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
