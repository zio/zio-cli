package zio.cli

import zio.cli.HelpDoc.h1
import zio.cli.ValidationErrorType.CommandMismatch
import zio.cli.oauth2.OAuth2PlatformSpecific
import zio.{Chunk, IO, ZIO}

/**
 * A `Command` represents a command in a command-line application. Every command-line application will have at least one
 * command: the application itself. Other command-line applications may support multiple commands.
 */
sealed trait Command[+A] extends Parameter with Named { self =>
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

  def getSubcommands: Map[String, Command[_]]

  def helpDoc: HelpDoc

  final def map[B](f: A => B): Command[B] = Command.Map(self, f)

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

  lazy val tag: String = "command"
}

object Command {
  def unCluster(args: List[String]): List[String] = {
    def isClusteredOption(value: String): Boolean = value.trim.matches("^-{1}([^-]{2,}|$)")

    args.flatMap { arg =>
      if (isClusteredOption(arg))
        arg.substring(1).map(c => s"-$c")
      else arg :: Nil
    }
  }

  private def splitForcedArgs(args: List[String]): (List[String], List[String]) = {
    val (remainingArgs, forcedArgs) = args.span(_ != "--")
    (remainingArgs, forcedArgs.drop(1))
  }

  final case class Single[OptionsType, ArgsType](
    val name: String,
    help: HelpDoc,
    options: Options[OptionsType],
    args: Args[ArgsType]
  ) extends Command[(OptionsType, ArgsType)]
      with Pipeline
      with Named { self =>

    override lazy val shortDesc = help.getSpan.text

    lazy val helpDoc: HelpDoc = {
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

      val oauth2Section = OAuth2PlatformSpecific.oauth2HelpSection(options)

      helpHeader + argumentsSection + optionsSection + oauth2Section
    }

    lazy val names: Set[String] = Set(self.name)

    def parse(
      args: List[String],
      conf: CliConfig
    ): IO[ValidationError, CommandDirective[(OptionsType, ArgsType)]] = {
      val parseBuiltInArgs =
        if (args.headOption.exists(conf.normalizeCase(_) == conf.normalizeCase(self.name)))
          BuiltInOption
            .builtInOptions(self, self.synopsis, self.helpDoc)
            .validate(args, conf)
            .mapBoth(_.error, _._2)
            .some
            .map(CommandDirective.BuiltIn)
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
          tuple1                              = splitForcedArgs(commandOptionsAndArgs)
          (optionsAndArgs, forcedCommandArgs) = tuple1
          tuple2                             <- self.options.validate(unCluster(optionsAndArgs), conf)
          (commandArgs, optionsType)          = tuple2
          tuple                              <- self.args.validate(commandArgs ++ forcedCommandArgs, conf)
          (argsLeftover, argsType)            = tuple
        } yield CommandDirective.userDefined(argsLeftover, (optionsType, argsType))

      parseBuiltInArgs orElse parseUserDefinedArgs
    }

    lazy val synopsis: UsageSynopsis =
      UsageSynopsis.Named(List(self.name), None) + self.options.synopsis + self.args.synopsis

    def pipeline = ("", List(options, args))

    def getSubcommands: Predef.Map[String, Command[_]] = Predef.Map(self.name -> self)
  }

  final case class Map[A, B](command: Command[A], f: A => B) extends Command[B] with Pipeline with Wrap { self =>

    override lazy val shortDesc = command.shortDesc
    lazy val helpDoc            = self.command.helpDoc

    lazy val names: Set[String] = self.command.names

    def parse(
      args: List[String],
      conf: CliConfig
    ): IO[ValidationError, CommandDirective[B]] =
      self.command.parse(args, conf).map(_.map(f))

    lazy val synopsis: UsageSynopsis = self.command.synopsis

    override def wrapped: Command[A] = self.command

    def pipeline = ("", List(command))

    def getSubcommands: Predef.Map[String, Command[_]] = self.command.getSubcommands
  }

  final case class OrElse[A](left: Command[A], right: Command[A]) extends Command[A] with Alternatives { self =>
    lazy val helpDoc: HelpDoc = self.left.helpDoc + self.right.helpDoc

    lazy val names: Set[String] = self.left.names ++ self.right.names

    def parse(
      args: List[String],
      conf: CliConfig
    ): IO[ValidationError, CommandDirective[A]] =
      self.left.parse(args, conf).catchSome { case ValidationError(CommandMismatch, _) => self.right.parse(args, conf) }

    lazy val synopsis: UsageSynopsis = UsageSynopsis.Mixed

    override val alternatives = List(left, right)

    def getSubcommands: Predef.Map[String, Command[_]] = self.left.getSubcommands ++ self.right.getSubcommands
  }

  final case class Subcommands[A, B](parent: Command[A], child: Command[B]) extends Command[(A, B)] with Pipeline {
    self =>

    override lazy val shortDesc = parent.shortDesc

    lazy val helpDoc = {
      def getMaxSynopsisLength[C](command: Command[C]): Int =
        command match {
          case OrElse(left, right) =>
            Math.max(getMaxSynopsisLength(left), getMaxSynopsisLength(right))
          case Single(_, _, _, _) =>
            command.synopsis.helpDoc.getSpan.size
          case Map(cmd, _) =>
            getMaxSynopsisLength(cmd)
          case Subcommands(parent, _) =>
            getMaxSynopsisLength(parent)
        }

      def subcommandsDesc[C](command: Command[C], maxSynopsisLength: Int): HelpDoc =
        command match {
          case OrElse(left, right) =>
            HelpDoc.enumeration(subcommandsDesc(left, maxSynopsisLength), subcommandsDesc(right, maxSynopsisLength))
          case Single(_, desc, _, _) =>
            val synopsisSpan = command.synopsis.helpDoc.getSpan
            HelpDoc.p {
              HelpDoc.Span.spans(
                synopsisSpan,
                HelpDoc.Span.text(" " * (maxSynopsisLength - synopsisSpan.size + 2)),
                desc.getSpan
              )
            }
          case Map(cmd, _) =>
            subcommandsDesc(cmd, maxSynopsisLength)
          case Subcommands(parent, _) =>
            subcommandsDesc(parent, maxSynopsisLength)
        }

      self.parent.helpDoc + HelpDoc.h1("Commands") + subcommandsDesc(self.child, getMaxSynopsisLength(self.child))
    }

    lazy val names: Set[String] = self.parent.names

    def parse(
      args: List[String],
      conf: CliConfig
    ): IO[ValidationError, CommandDirective[(A, B)]] = {
      lazy val helpDirectiveForChild = {

        val subCommandNamesAndAliases =
          parent.getSubcommands.values.collect { case Single(name, _, options, _) =>
            (name, (options.nameAndAliases, options.valueCount))
          }.toMap

        val safeTail = args match {
          case Nil => Nil
          case first :: options :: tail =>
            subCommandNamesAndAliases.get(first) match {
              case Some((nameAndAliases, count)) =>
                if (nameAndAliases.contains(options))
                  tail.drop(count)
                else
                  options :: tail
              case None =>
                options :: tail
            }
          case _ :: tail => tail
        }
        self.child
          .parse(safeTail, conf)
          .collect(ValidationError(ValidationErrorType.InvalidArgument, HelpDoc.empty)) {
            case CommandDirective.BuiltIn(BuiltInOption.ShowHelp(synopsis, helpDoc)) =>
              val parentName = self.names.headOption.getOrElse("")
              CommandDirective.builtIn {
                BuiltInOption.ShowHelp(
                  UsageSynopsis.Named(List(parentName), None) + synopsis,
                  helpDoc
                )
              }
          }
      }

      val helpDirectiveForParent =
        ZIO.succeed(CommandDirective.builtIn(BuiltInOption.ShowHelp(self.synopsis, self.helpDoc)))

      val wizardDirectiveForChild = {
        val safeTail = args match {
          case Nil       => Nil
          case _ :: tail => tail
        }
        self.child
          .parse(safeTail, conf)
          .collect(ValidationError(ValidationErrorType.InvalidArgument, HelpDoc.empty)) {
            case directive @ CommandDirective.BuiltIn(BuiltInOption.ShowWizard(_)) => directive
          }
      }

      val wizardDirectiveForParent =
        ZIO.succeed(CommandDirective.builtIn(BuiltInOption.ShowWizard(self)))

      self.parent
        .parse(args, conf)
        .debug("Subcommand parent parse")
        .flatMap {
          case CommandDirective.BuiltIn(BuiltInOption.ShowHelp(_, _)) =>
            helpDirectiveForChild orElse helpDirectiveForParent
          case CommandDirective.BuiltIn(BuiltInOption.ShowWizard(_)) =>
            wizardDirectiveForChild orElse wizardDirectiveForParent
          case builtIn @ CommandDirective.BuiltIn(_) => ZIO.succeed(builtIn)
          case CommandDirective.UserDefined(leftover, a) if leftover.nonEmpty =>
            self.child
              .parse(leftover, conf)
              .mapBoth(
                {
                  case ValidationError(CommandMismatch, _) =>
                    val parentName      = self.names.headOption.getOrElse("")
                    val subCommandNames = Chunk.fromIterable(self.getSubcommands.keys).map(n => s"'$n'")
                    val oneOf           = if (subCommandNames.size == 1) "" else " one of"
                    ValidationError(
                      CommandMismatch,
                      HelpDoc.p(
                        s"Invalid subcommand for ${parentName}. Use$oneOf ${subCommandNames.mkString(", ")}"
                      )
                    )
                  case other: ValidationError => other
                },
                _.map((a, _))
              )
          case _ =>
            helpDirectiveForParent
        }
        .catchSome {
          case _ if args.isEmpty =>
            helpDirectiveForParent
        }
    }

    lazy val synopsis: UsageSynopsis = self.parent.synopsis + self.child.synopsis

    def pipeline = ("", List(parent, child))

    def getSubcommands: Predef.Map[String, Command[_]] = self.child.getSubcommands
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
