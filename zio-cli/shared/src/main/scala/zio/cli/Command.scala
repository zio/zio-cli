package zio.cli

import zio.{ IO, ZIO }
import zio.cli.HelpDoc.h1
import zio.cli.HelpDoc.Span

/**
 * A `Command` represents a command in a command-line application. Every command-line application
 * will have at least one command: the application itself. Other command-line applications may
 * support multiple commands.
 */
sealed trait Command[+A] { self =>
  final def |[A1 >: A](that: Command[A1]): Command[A1] = Command.Fallback(self, that)

  final def as[B](b: => B): Command[B] = self.map(_ => b)

  def helpDoc: HelpDoc

  final def map[B](f: A => B): Command[B] = Command.Map(self, f)

  def names: Set[String]

  final def orElse[A1 >: A](that: Command[A1]): Command[A1] = self | that

  final def orElseEither[B](that: Command[B]): Command[Either[A, B]] = self.map(Left(_)) | that.map(Right(_))

  def parse(args: List[String], conf: CliConfig): IO[HelpDoc, CommandDirective[A]]

  final def subcommands[B](that: Command[B]): Command[(A, B)] = Command.Subcommands(self, that)

  final def subcommands[B](c1: Command[B], c2: Command[B], cs: Command[B]*): Command[(A, B)] =
    subcommands(cs.foldLeft(c1 | c2)(_ | _))

  def synopsis: UsageSynopsis
}

object Command {
  final case class Single[OptionsType, ArgsType](
    name: String,
    description: HelpDoc,
    options: Options[OptionsType],
    args: Args[ArgsType]
  ) extends Command[(OptionsType, ArgsType)] { self =>

    /**
     * Built-in options supported by the command, such as "--help".
     */
    lazy val builtInOptions: Options[Option[BuiltInOption]] =
      BuiltInOption.builtInOptions(helpDoc, completions(_))

    def builtIn(
      args: List[String],
      conf: CliConfig
    ): IO[Option[HelpDoc], CommandDirective[(OptionsType, ArgsType)]] =
      builtInOptions.validate(args, conf).map(_._2).some.map(CommandDirective.BuiltIn(_))

    def completions(shellType: ShellType): Set[List[String]] = ???

    def helpDoc: HelpDoc = {
      val descriptionsSection = {
        val desc = description

        if (desc.isEmpty) HelpDoc.Empty
        else h1("description") + desc
      }

      val argumentsSection = {
        val args = self.args.helpDoc

        if (args == HelpDoc.Empty) HelpDoc.Empty
        else h1("arguments") + self.args.helpDoc
      }

      val optionsSection = {
        val opts = (self.options :: BuiltInOption.builtInOptions).helpDoc

        if (opts == HelpDoc.Empty) HelpDoc.Empty
        else h1("options") + opts
      }

      descriptionsSection + argumentsSection + optionsSection
    }

    def names: Set[String] = Set(name)

    final def parse(
      args: List[String],
      conf: CliConfig
    ): IO[HelpDoc, CommandDirective[(OptionsType, ArgsType)]] =
      builtIn(args, conf).catchAll(_ => userDefined(args, conf)).catchSome {
        case _ if args.isEmpty => ZIO.succeed(CommandDirective.BuiltIn(BuiltInOption.ShowHelp(helpDoc)))
      }

    def synopsis: UsageSynopsis =
      UsageSynopsis.Named(name, None) + options.synopsis + args.synopsis

    def userDefined(
      args: List[String],
      conf: CliConfig
    ): IO[HelpDoc, CommandDirective[(OptionsType, ArgsType)]] =
      for {
        tuple               <- self.options.validate(args, conf)
        (args, optionsType) = tuple
        tuple               <- self.args.validate(args, conf)
        (args, argsType)    = tuple
        _ <- ZIO.when(args.nonEmpty)(
              ZIO.fail(HelpDoc.p(Span.error(s"Unexpected arguments for command ${name}: ${args}")))
            )
      } yield CommandDirective.userDefined(args, (optionsType, argsType))
  }

  final case class Map[A, B](command: Command[A], f: A => B) extends Command[B] {
    def helpDoc = command.helpDoc

    def names: Set[String] = command.names

    final def parse(
      args: List[String],
      conf: CliConfig
    ): IO[HelpDoc, CommandDirective[B]] =
      command.parse(args, conf).map(_.map(f))

    def synopsis: UsageSynopsis = command.synopsis
  }
  final case class Fallback[A](left: Command[A], right: Command[A]) extends Command[A] {
    def helpDoc = left.helpDoc + right.helpDoc

    def names: Set[String] = left.names ++ right.names

    final def parse(
      args: List[String],
      conf: CliConfig
    ): IO[HelpDoc, CommandDirective[A]] =
      left.parse(args, conf) orElse right.parse(args, conf)

    def synopsis: UsageSynopsis = UsageSynopsis.Mixed
  }
  final case class Subcommands[A, B](parent: Command[A], child: Command[B]) extends Command[(A, B)] {
    def helpDoc = parent.helpDoc + h1("subcommands") + child.helpDoc

    def names: Set[String] = parent.names

    final def parse(
      args: List[String],
      conf: CliConfig
    ): IO[HelpDoc, CommandDirective[(A, B)]] =
      parent.parse(args, conf).flatMap {
        case x @ CommandDirective.BuiltIn(_) => ZIO.succeed(x)
        case CommandDirective.UserDefined(leftover, a) =>
          child.parse(leftover, conf).map(_.map(b => (a, b)))
      }

    def synopsis: UsageSynopsis = parent.synopsis
  }

  /**
   * Construct a new command.
   */
  def apply[OptionsType, ArgsType](
    name: String,
    options: Options[OptionsType],
    args: Args[ArgsType],
    helpDoc: HelpDoc = HelpDoc.Empty
  ): Command[(OptionsType, ArgsType)] = Single(name, helpDoc, options, args)
}
