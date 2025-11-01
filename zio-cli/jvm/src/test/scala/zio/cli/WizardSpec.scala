package zio.cli

import zio.{ZIO, UIO}
import zio.test._
import zio.test.TestConsole
import zio.test.Gen
import zio.cli.Options._
import zio.cli.oauth2.OAuth2AuxiliaryOptions
import zio.cli.oauth2.OAuth2Provider
import zio.cli.Args.Variadic

object WizardSpec extends ZIOSpecDefault {

  def parameterTrait(param: Parameter) =
    param match {
      case _: Alternatives => "alternatives"
      case _: Input        => "input"
      case _: Pipeline     => "pipeline"
      case _: Named        => "named"
      case _: Wrap         => "wrap"
      case _               => ""
    }

  def argsType(args: Args[_]) =
    args match {
      case Args.Both(_, _)      => "pipeline"
      case Args.Empty           => "pipeline"
      case Args.Map(_, _)       => "pipeline"
      case Args.Single(_, _, _) => "input"
      case Variadic(_, _, _)    => "input"
    }

  def commandType(command: Command[_]) =
    command match {
      case Command.Map(_, _)          => "pipeline"
      case Command.OrElse(_, _)       => "alternatives"
      case Command.Single(_, _, _, _) => "pipeline"
      case Command.Subcommands(_, _)  => "pipeline"
    }

  def optionsType(options: Options[_]) =
    options match {
      case Both(_, _)             => "pipeline"
      case Empty                  => "pipeline"
      case Options.Map(_, _)      => "pipeline"
      case Options.OrElse(_, _)   => "alternatives"
      case KeyValueMap(_)         => "input"
      case Single(_, _, _, _, _)  => "input"
      case WithDefault(_, _)      => "input"
      case OAuth2Options(_, _, _) => "wrap"
    }

  def parameterType(param: Parameter) =
    param match {
      case options: Options[_] => optionsType(options)
      case args: Args[_]       => argsType(args)
      case command: Command[_] => commandType(command)
      case _                   => ""
    }

  val anyOptions =
    Gen.fromIterable(
      List(
        Both(options("a"), options("b")),
        Empty,
        Empty.map(_ => "a"),
        Options.OrElse(options("a"), options("b")),
        KeyValueMap(Options.Single("a", Vector.empty, PrimType.Text)),
        options("a"),
        WithDefault(options("a"), "default"),
        OAuth2Options(
          OAuth2Provider.Github("1234567890"),
          Nil,
          Options.Empty.map(_ => OAuth2AuxiliaryOptions(java.nio.file.Path.of("a")))
        )
      )
    )

  val anyArgs =
    Gen.fromIterable(List(Args.Both(Args.Empty, Args.Empty), Args.Empty, Args.Empty.map(_ => "a"), Args.text))

  val anyCommand =
    Gen.fromIterable(
      List(
        Command("a"),
        Command("a").subcommands(Command("b")),
        Command("a").map(_ => "b"),
        Command.OrElse(Command("a"), Command("b"))
      )
    )

  val anyParameter = Gen.oneOf(
    anyOptions,
    anyArgs,
    anyCommand
  )

  def options(name: String) = Options.text(name)

  def single(name: String, options: Options[_] = Options.Empty) = Command(name, options)

  val alternatives = Command("command").subcommands(single("aa"), single("bb"), single("cc"))

  def testWizard(command: Command[_], parameters: List[String]): UIO[String] = {
    val wizard = Wizard(command, CliConfig.default, HelpDoc.empty)
    for {
      _            <- TestConsole.feedLines(parameters: _*)
      finalCommand <- wizard.execute.catchAll { case Wizard.QuitException() =>
                        ZIO.succeed(List("quitting..."))
                      }
    } yield finalCommand.mkString(" ")
  }

  override def spec =
    suite("Wizard spec")(
      test("Parameter")(
        check(anyParameter) { param =>
          assertTrue(parameterTrait(param) == parameterType(param))
        }
      ),
      test("Alternatives")(
        for {
          res <- testWizard(alternatives, List("bb"))
        } yield assertTrue(res == "bb")
      ),
      test("Input and Pipeline")(
        for {
          res <- testWizard(single("command", options("opt")), List("sample"))
        } yield assertTrue(res == "--opt sample")
      ),
      test("Wrap")(
        for {
          res <- testWizard(single("command", options("opt")).map(_ => ""), List("sample"))
        } yield assertTrue(res == "--opt sample")
      ),
      test("Restart")(
        for {
          res <- testWizard(
                   single("command1", options("opt1") ++ options("opt2")).map(_ => ""),
                   List("sample", "restart", "secondtry", "sample")
                 )
        } yield assertTrue(res == "--opt1 secondtry --opt2 sample")
      ),
      test("Quit")(
        for {
          res <- testWizard(single("command", options("opt")).map(_ => ""), List("quit"))
        } yield assertTrue(res == "quitting...")
      )
    )

}
