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

  final def withHelp(help: String): Command[A] =
    self.withHelp(HelpDoc.p(help))

  final def withHelp(help: HelpDoc): Command[A] =
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

  final def subcommands[B](that: Command[B])(implicit ev: Reducable[A, B]): Command[ev.Out] =
    Command.Subcommands(self, that).map(ev.fromTuple2(_))

  final def subcommands[B](c1: Command[B], c2: Command[B], cs: Command[B]*)(implicit
    ev: Reducable[A, B]
  ): Command[ev.Out] =
    self.subcommands(cs.foldLeft(c1 | c2)(_ | _))(ev)

  def synopsis: UsageSynopsis
}

object Command {
  // TODO this should not be here
  def unCluster(args: List[String]): List[String] = {
    def isClusteredOption(value: String): Boolean = value.trim.matches("^-{1}([^-]{2,}|$)")

    args.flatMap { arg =>
      if (isClusteredOption(arg))
        arg.substring(1).map(c => s"-$c")
      else arg :: Nil
    }
  }

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
      BuiltInOption.builtInOptions(self.synopsis, self.helpDoc)

    def builtIn(
      args: List[String],
      conf: CliConfig
    ): IO[Option[HelpDoc], CommandDirective[(OptionsType, ArgsType)]] =
      builtInOptions.validate(args, conf).mapBoth(_.error, _._2).some.map(CommandDirective.BuiltIn)

    def helpDoc: HelpDoc = {
      val helpHeader = {
        val desc = self.help

        if (desc.isEmpty) HelpDoc.Empty
        else h1("description") + desc
      }

      val argumentsSection = {
        val args = self.args.helpDoc

        if (args == HelpDoc.Empty) HelpDoc.Empty
        else h1("arguments") + self.args.helpDoc
      }

      val optionsSection = {
        val opts = self.options.helpDoc

        if (opts == HelpDoc.Empty) HelpDoc.Empty
        else h1("options") + opts
      }

      helpHeader + argumentsSection + optionsSection
    }

    def names: Set[String] = Set(self.name)

    def parse(
      args: List[String],
      conf: CliConfig
    ): IO[ValidationError, CommandDirective[(OptionsType, ArgsType)]] = {
      val parseBuiltInArgs =
        if (args.headOption.exists(conf.normalizeCase(_) == conf.normalizeCase(self.name))) self.builtIn(args, conf)
        else ZIO.fail(None)

      val parseUserDefinedArgs =
        for {
          commandOptionsAndArgs <- args match {
                                     case head :: tail =>
                                       ZIO
                                         .succeed(tail)
                                         .when(conf.normalizeCase(head) == conf.normalizeCase(self.name))
                                         .some
                                         .orElseFail {
                                           ValidationError(
                                             ValidationErrorType.CommandMismatch,
                                             HelpDoc.p(s"Missing command name: ${self.name}")
                                           )
                                         }
                                     case Nil =>
                                       ZIO.fail {
                                         ValidationError(
                                           ValidationErrorType.CommandMismatch,
                                           HelpDoc.p(s"Missing command name: ${self.name}")
                                         )
                                       }
                                   }
          tuple                     <- self.options.validate(unCluster(commandOptionsAndArgs), conf)
          (commandArgs, optionsType) = tuple
          tuple                     <- self.args.validate(commandArgs, conf)
          (argsLeftover, argsType)   = tuple
        } yield CommandDirective.userDefined(argsLeftover, (optionsType, argsType))

      parseBuiltInArgs orElse parseUserDefinedArgs
    }

    def synopsis: UsageSynopsis =
      UsageSynopsis.Named(self.name, None) + self.options.synopsis + self.args.synopsis
  }

  final case class Map[A, B](command: Command[A], f: A => B) extends Command[B] { self =>
    def helpDoc = self.command.helpDoc

    def names: Set[String] = self.command.names

    def parse(
      args: List[String],
      conf: CliConfig
    ): IO[ValidationError, CommandDirective[B]] =
      self.command.parse(args, conf).map(_.map(f))

    def synopsis: UsageSynopsis = self.command.synopsis
  }

  final case class OrElse[A](left: Command[A], right: Command[A]) extends Command[A] { self =>
    def helpDoc: HelpDoc = self.left.helpDoc + self.right.helpDoc

    def names: Set[String] = self.left.names ++ self.right.names

    def parse(
      args: List[String],
      conf: CliConfig
    ): IO[ValidationError, CommandDirective[A]] =
      self.left.parse(args, conf).catchSome { case ValidationError(CommandMismatch, _) => self.right.parse(args, conf) }

    def synopsis: UsageSynopsis = UsageSynopsis.Mixed
  }

  final case class Subcommands[A, B](parent: Command[A], child: Command[B]) extends Command[(A, B)] { self =>
    def helpDoc = {
      def getHelpDescription(helpDoc: HelpDoc): HelpDoc.Span =
        helpDoc match {
          case HelpDoc.Header(value, _) => value
          case HelpDoc.Paragraph(value) => value
          case _                        => HelpDoc.Span.space
        }

      def subcommandsDesc[C](command: Command[C]): HelpDoc =
        command match {
          case OrElse(left, right) =>
            HelpDoc.enumeration(subcommandsDesc(left), subcommandsDesc(right))
          case Single(name, desc, _, _) =>
            HelpDoc.p {
              HelpDoc.Span.spans(
                getHelpDescription(command.synopsis.helpDoc),
                HelpDoc.Span.text("\t"), // TODO correctly calculate the number of tabs
                getHelpDescription(desc)
              )
            }
          case Map(cmd, _) =>
            subcommandsDesc(cmd)
          case _ =>
            HelpDoc.empty
        }

      self.parent.helpDoc + HelpDoc.h1("Commands") + subcommandsDesc(self.child)
    }

    def names: Set[String] = self.parent.names

    def parse(
      args: List[String],
      conf: CliConfig
    ): IO[ValidationError, CommandDirective[(A, B)]] = {
      val helpDirectiveForChild =
        self.child
          .parse(args.tail, conf)
          .collect(ValidationError(ValidationErrorType.InvalidArgument, HelpDoc.empty)) {
            case CommandDirective.BuiltIn(BuiltInOption.ShowHelp(synopsis, helpDoc)) =>
              val parentName = self.names.headOption.getOrElse("")
              CommandDirective.builtIn {
                BuiltInOption.ShowHelp(
                  UsageSynopsis.Named(parentName, None) + synopsis,
                  helpDoc
                )
              }
          }

      val helpDirectiveForParent =
        ZIO.succeed(CommandDirective.builtIn(BuiltInOption.ShowHelp(self.synopsis, self.helpDoc)))

      self.parent
        .parse(args, conf)
        .flatMap {
          case CommandDirective.BuiltIn(BuiltInOption.ShowHelp(_, _)) =>
            helpDirectiveForChild orElse helpDirectiveForParent
          case builtIn @ CommandDirective.BuiltIn(_) => ZIO.succeed(builtIn)
          case CommandDirective.UserDefined(leftover, a) if leftover.nonEmpty =>
            self.child.parse(leftover, conf).map(_.map((a, _)))
          case _ =>
            helpDirectiveForParent
        }
        .catchSome {
          case _ if args.isEmpty =>
            helpDirectiveForParent
        }
    }

    def synopsis: UsageSynopsis = self.parent.synopsis + self.child.synopsis
  }

  /**
   * Construct a new command.
   */
  def apply[OptionsType, ArgsType](
    name: String,
    options: Options[OptionsType],
    args: Args[ArgsType]
  )(implicit ev: Reducable[OptionsType, ArgsType]): Command[ev.Out] =
    Single(name, HelpDoc.empty, options, args).map(ev.fromTuple2(_))

  def apply[OptionsType](
    name: String,
    options: Options[OptionsType]
  )(implicit ev: Reducable[OptionsType, Unit]): Command[ev.Out] =
    Single(name, HelpDoc.empty, options, Args.none).map(ev.fromTuple2(_))

  def apply[ArgsType](
    name: String,
    args: Args[ArgsType]
  )(implicit ev: Reducable[Unit, ArgsType]): Command[ev.Out] =
    Single(name, HelpDoc.empty, Options.none, args).map(ev.fromTuple2(_))

  def apply(
    name: String
  )(implicit ev: Reducable[Unit, Unit]): Command[ev.Out] =
    Single(name, HelpDoc.empty, Options.none, Args.none).map(ev.fromTuple2(_))
}
