package zio.cli

import zio.{IO, ZIO}
import zio.cli.HelpDoc.h1
import zio.cli.ValidationErrorType.CommandMismatch

/**
 * A `Command` represents a command in a command-line application. Every command-line application
 * will have at least one command: the application itself. Other command-line applications may
 * support multiple commands.
 */
sealed trait Command[+A] { self =>
  final def |[A1 >: A](that: Command[A1]): Command[A1] = Command.OrElse(self, that)

  final def as[B](b: => B): Command[B] = self.map(_ => b)

  def withHelp(help: String): Command[A] =
    withHelp(HelpDoc.p(help))

  def withHelp(help: HelpDoc): Command[A] =
    self match {
      case single: Command.Single[_, _] =>
        single.copy(help = help).asInstanceOf[Command[A]]

      case Command.Map(command, f) =>
        Command.Map(command.withHelp(help), f)

      case Command.OrElse(left, right) =>
        Command.OrElse(
          left.withHelp(help),
          right.withHelp(help)
        ) // if the left and right also have help, it gets overwritten by this, maybe not the best idea

      case subcommands: Command.Subcommands[_, _] =>
        subcommands.copy(parent = subcommands.parent.withHelp(help)).asInstanceOf[Command[A]]
    }

  def helpDoc: HelpDoc

  final def map[B](f: A => B): Command[B] = Command.Map(self, f)

  def names: Set[String]

  final def orElse[A1 >: A](that: Command[A1]): Command[A1] = self | that

  final def orElseEither[B](that: Command[B]): Command[Either[A, B]] = self.map(Left(_)) | that.map(Right(_))

  def parse(args: List[String], conf: CliConfig): IO[ValidationError, CommandDirective[A]]

  final def subcommands[B](that: Command[B]): Command[(A, B)] = Command.Subcommands(self, that)

  final def subcommands[B](c1: Command[B], c2: Command[B], cs: Command[B]*): Command[(A, B)] =
    subcommands(cs.foldLeft(c1 | c2)(_ | _))

  def synopsis: UsageSynopsis
}

object Command {

  final case class Single[OptionsType, ArgsType](
    name: String,
    help: HelpDoc,
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
      builtInOptions.validate(args, conf).mapBoth(_.error, _._2).some.map(CommandDirective.BuiltIn(_))

    def completions(shellType: ShellType): Set[List[String]] = ???

    def helpDoc: HelpDoc = {
      val helpHeader = {
        val desc = help

        if (desc.isEmpty) HelpDoc.Empty
        else h1("description") + desc
      }

      val argumentsSection = {
        val args = self.args.helpDoc

        if (args == HelpDoc.Empty) HelpDoc.Empty
        else h1("arguments") + self.args.helpDoc
      }

      val optionsSection = {
        val opts = (self.options).helpDoc

        if (opts == HelpDoc.Empty) HelpDoc.Empty
        else h1("options") + opts
      }

      helpHeader + argumentsSection + optionsSection
    }

    def names: Set[String] = Set(name)

    def isClusteredOption(value: String): Boolean = value.trim.matches("^-{1}([^-]{2,}|$)")

    def unCluster(args: List[String]): List[String] = args.flatMap { arg =>
      if (isClusteredOption(arg))
        arg.substring(1).map(c => s"-$c")
      else arg :: Nil
    }

    def parse(
      args: List[String],
      conf: CliConfig
    ): IO[ValidationError, CommandDirective[(OptionsType, ArgsType)]] =
      parseBuiltInArgs(args, conf) orElse userDefined(args, conf)

    def parseBuiltInArgs(
      args: List[String],
      conf: CliConfig
    ): IO[Option[HelpDoc], CommandDirective[(OptionsType, ArgsType)]] =
      if (args.headOption.map(conf.normalizeCase(_) == conf.normalizeCase(name)).getOrElse(false))
        builtIn(args, conf)
      else
        IO.fail(None)

    def synopsis: UsageSynopsis =
      UsageSynopsis.Named(name, None) + options.synopsis + args.synopsis

    def userDefined(
      args: List[String],
      conf: CliConfig
    ): IO[ValidationError, CommandDirective[(OptionsType, ArgsType)]] =
      for {
        args <- args match {
                  case head :: tail =>
                    if (conf.normalizeCase(head) == conf.normalizeCase(name)) IO.succeed(tail)
                    else
                      IO.fail(
                        ValidationError(
                          ValidationErrorType.CommandMismatch,
                          HelpDoc.p(s"Unexpected command name: ${args.headOption}")
                        )
                      )
                  case Nil =>
                    IO.fail(
                      ValidationError(ValidationErrorType.CommandMismatch, HelpDoc.p(s"Missing command name: ${name}"))
                    )
                }
        tuple              <- self.options.validate(unCluster(args), conf)
        (args, optionsType) = tuple
        tuple <- self.args
                   .validate(args, conf)
                   .mapError(helpDoc => ValidationError(ValidationErrorType.InvalidArgument, helpDoc))
        (args, argsType) = tuple
      } yield {
        CommandDirective.userDefined(args, (optionsType, argsType))
      }
  }

  final case class Map[A, B](command: Command[A], f: A => B) extends Command[B] {
    def helpDoc = command.helpDoc

    def names: Set[String] = command.names

    def parse(
      args: List[String],
      conf: CliConfig
    ): IO[ValidationError, CommandDirective[B]] =
      command.parse(args, conf).map(_.map(f))

    def synopsis: UsageSynopsis = command.synopsis
  }

  final case class OrElse[A](left: Command[A], right: Command[A]) extends Command[A] {
    def helpDoc: HelpDoc = left.helpDoc + right.helpDoc

    def names: Set[String] = left.names ++ right.names

    def parse(
      args: List[String],
      conf: CliConfig
    ): IO[ValidationError, CommandDirective[A]] =
      left.parse(args, conf).catchSome { case ValidationError(CommandMismatch, _) => right.parse(args, conf) }

    def synopsis: UsageSynopsis = UsageSynopsis.Mixed
  }

  final case class Subcommands[A, B](parent: Command[A], child: Command[B]) extends Command[(A, B)] { self =>
    def getHelpDescription(h: HelpDoc): HelpDoc.Span =
      h match {
        case HelpDoc.Header(value, _) => value
        case HelpDoc.Paragraph(value) => value
        case _                        => HelpDoc.Span.space
      }

    def subcommandsDesc[C](c: Command[C]): HelpDoc =
      c match {
        case OrElse(left, right) =>
          HelpDoc.enumeration(subcommandsDesc(left), subcommandsDesc(right))
        case Single(name, desc, _, _) =>
          HelpDoc.p(HelpDoc.Span.spans(HelpDoc.Span.text(name), HelpDoc.Span.text(" \t "), getHelpDescription(desc)))
        case Map(cmd, _) =>
          subcommandsDesc(cmd)
        case c =>
          HelpDoc.empty
      }

    def helpDoc =
      parent.helpDoc + HelpDoc.h1("Subcommands") + subcommandsDesc(child)

    def names: Set[String] = parent.names

    def parse(
      args: List[String],
      conf: CliConfig
    ): IO[ValidationError, CommandDirective[(A, B)]] =
      parent
        .parse(args, conf)
        .flatMap {
          // TODO: Try removing this. `git remote --help` was the same as `git --help` when I tested.
          case CommandDirective.BuiltIn(x) =>
            x match {
              case BuiltInOption.ShowHelp(_) =>
                for {
                  help <- (child.parse(args.tail, conf) orElse ZIO.succeed(
                            CommandDirective.builtIn(BuiltInOption.ShowHelp(self.helpDoc))
                          ))
                  help <- help match {
                            case CommandDirective.BuiltIn(BuiltInOption.ShowHelp(h)) => IO.succeed(h)
                            case _ =>
                              IO.fail(
                                ValidationError(
                                  ValidationErrorType.InvalidArgument,
                                  HelpDoc.empty
                                )
                              )
                          }
                } yield {
                  CommandDirective.builtIn(BuiltInOption.ShowHelp(help))
                }

              case x => ZIO.succeed(CommandDirective.builtIn(x))
            }

          case CommandDirective.UserDefined(leftover, a) if leftover.nonEmpty =>
            child.parse(leftover, conf).map(_.map(b => (a, b)))

          case _ =>
            ZIO.succeed(CommandDirective.builtIn(BuiltInOption.ShowHelp(helpDoc)))
        }
        .catchSome {
          case _ if args.isEmpty => ZIO.succeed(CommandDirective.BuiltIn(BuiltInOption.ShowHelp(self.helpDoc)))
        }

    def synopsis: UsageSynopsis = parent.synopsis + child.synopsis
  }

  /**
   * Construct a new command.
   */
  def apply[OptionsType, ArgsType](
    name: String,
    options: Options[OptionsType],
    args: Args[ArgsType]
  ): Command[(OptionsType, ArgsType)] =
    Single(name, HelpDoc.empty, options, args)

  def apply[OptionsType](
    name: String,
    options: Options[OptionsType]
  ): Command[(OptionsType, Unit)] =
    Single(name, HelpDoc.empty, options, Args.none)

  def apply[ArgsType](
    name: String,
    args: Args[ArgsType]
  ): Command[(Unit, ArgsType)] =
    Single(name, HelpDoc.empty, Options.none, args)

  def apply(
    name: String
  ): Command[(Unit, Unit)] = Single(name, HelpDoc.empty, Options.none, Args.none)
}
