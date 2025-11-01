package zio.cli

import zio.cli.HelpDoc.h1
import zio.cli.ValidationErrorType.CommandMismatch
import zio.cli.oauth2.OAuth2PlatformSpecific
import zio.{Chunk, IO, ZIO}

/**
 * A `Command` represents a command in a command-line application. Every command-line application will have at least one
 * command: the application it Other command-line applications may support multiple commands.
 */
sealed trait Command[+A] extends Parameter with Named { self =>

  final def |[A1 >: A](that: Command[A1]): Command[A1] = Command.OrElse(self, that)

  final def as[B](b: => B): Command[B] = map(_ => b)

  final def withHelp(help: String): Command[A] =
    withHelp(HelpDoc.p(help))

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

  final def orElseEither[B](that: Command[B]): Command[Either[A, B]] = map(Left(_)) | that.map(Right(_))

  def parse(args: List[String], conf: CliConfig): IO[ValidationError, CommandDirective[A]]

  final def subcommands[B](that: Command[B])(implicit ev: Reducable[A, B]): Command[ev.Out] =
    Command.Subcommands(self, that).map(ev.fromTuple2(_))

  final def subcommands[B](c1: Command[B], c2: Command[B], cs: Command[B]*)(implicit
    ev: Reducable[A, B]
  ): Command[ev.Out] =
    subcommands(cs.foldLeft(c1 | c2)(_ | _))(ev)

  def synopsis: UsageSynopsis

  lazy val tag: String = "command"
}

object Command {

  private def splitForcedArgs(args: List[String]): (List[String], List[String]) = {
    val (remainingArgs, forcedArgs) = args.span(_ != "--")
    (remainingArgs, forcedArgs.drop(1))
  }

  final case class Single[OptionsType, ArgsType](
    name: String,
    help: HelpDoc,
    options: Options[OptionsType],
    args: Args[ArgsType]
  ) extends Command[(OptionsType, ArgsType)]
      with Pipeline
      with Named { self =>

    override lazy val shortDesc = help.getSpan.text

    lazy val helpDoc: HelpDoc = {
      val helpHeader = {
        val desc = help

        if (desc.isEmpty) HelpDoc.Empty
        else h1("description") + desc
      }

      val argumentsSection = {
        val argsHelp = args.helpDoc

        if (argsHelp == HelpDoc.Empty) HelpDoc.Empty
        else h1("arguments") + argsHelp
      }

      val optionsSection = {
        val optsHelp = options.helpDoc

        if (optsHelp == HelpDoc.Empty) HelpDoc.Empty
        else h1("options") + optsHelp
      }

      val oauth2Section = OAuth2PlatformSpecific.oauth2HelpSection(options)

      helpHeader + argumentsSection + optionsSection + oauth2Section
    }

    lazy val names: Set[String] = Set(name)

    def parse(
      args: List[String],
      conf: CliConfig
    ): IO[ValidationError, CommandDirective[(OptionsType, ArgsType)]] = {
      def parseBuiltInArgs(args: List[String]): IO[ValidationError, CommandDirective[Nothing]] =
        if (args.headOption.exists(conf.normalizeCase(_) == conf.normalizeCase(self.name))) {
          val options = BuiltInOption
            .builtInOptions(self, self.synopsis, self.helpDoc)
          Options
            .validate(options, args.tail, conf)
            .map(_._3)
            .someOrFail(
              ValidationError(
                ValidationErrorType.NoBuiltInMatch,
                HelpDoc.p(s"No built-in option was matched")
              )
            )
            .map(CommandDirective.BuiltIn)
        } else
          ZIO.fail(
            ValidationError(
              ValidationErrorType.CommandMismatch,
              HelpDoc.p(s"Missing command name: $name")
            )
          )

      val parseUserDefinedArgs =
        for {
          commandOptionsAndArgs <-
            args match {
              case head :: tail =>
                ZIO
                  .succeed(tail)
                  .when(conf.normalizeCase(head) == conf.normalizeCase(name))
                  .someOrFail {
                    ValidationError(
                      ValidationErrorType.CommandMismatch,
                      HelpDoc.p(s"Missing command name: $name")
                    )
                  }
              case Nil =>
                ZIO.fail {
                  ValidationError(
                    ValidationErrorType.CommandMismatch,
                    HelpDoc.p(s"Missing command name: $name")
                  )
                }
            }
          tuple1                                   = splitForcedArgs(commandOptionsAndArgs)
          (optionsAndArgs, forcedCommandArgs)      = tuple1
          tuple2                                  <- Options.validate(options, optionsAndArgs, conf)
          (optionsError, commandArgs, optionsType) = tuple2
          tuple                                   <- self.args.validate(commandArgs ++ forcedCommandArgs, conf).mapError(optionsError.getOrElse(_))
          (argsLeftover, argsType)                 = tuple
        } yield CommandDirective.userDefined(argsLeftover, (optionsType, argsType))

      val exhaustiveSearch: IO[ValidationError, CommandDirective[(OptionsType, ArgsType)]] =
        if (args.contains("--help") || args.contains("-h")) parseBuiltInArgs(List(name, "--help"))
        else if (args.contains("--wizard") || args.contains("-w")) parseBuiltInArgs(List(name, "--wizard"))
        else
          ZIO.fail(
            ValidationError(
              ValidationErrorType.CommandMismatch,
              HelpDoc.p(s"Missing command name: $name")
            )
          )

      val first = parseBuiltInArgs(args) orElse parseUserDefinedArgs

      first.catchSome { case e: ValidationError =>
        if (conf.finalCheckBuiltIn) exhaustiveSearch.catchSome { case _: ValidationError =>
          ZIO.fail(e)
        }
        else ZIO.fail(e)
      }
    }

    lazy val synopsis: UsageSynopsis =
      UsageSynopsis.Named(List(name), None) + options.synopsis + args.synopsis

    def pipeline = ("", List(options, args))

    def getSubcommands: Predef.Map[String, Command[_]] = Predef.Map(name -> self)
  }

  final case class Map[A, B](command: Command[A], f: A => B) extends Command[B] with Pipeline with Wrap {

    override lazy val shortDesc = command.shortDesc
    lazy val helpDoc            = command.helpDoc

    lazy val names: Set[String] = command.names

    def parse(
      args: List[String],
      conf: CliConfig
    ): IO[ValidationError, CommandDirective[B]] =
      command.parse(args, conf).map(_.map(f))

    lazy val synopsis: UsageSynopsis = command.synopsis

    override def wrapped: Command[A] = command

    def pipeline = ("", List(command))

    def getSubcommands: Predef.Map[String, Command[_]] = command.getSubcommands
  }

  final case class OrElse[A](left: Command[A], right: Command[A]) extends Command[A] with Alternatives {
    lazy val helpDoc: HelpDoc = left.helpDoc + right.helpDoc

    lazy val names: Set[String] = left.names ++ right.names

    def parse(
      args: List[String],
      conf: CliConfig
    ): IO[ValidationError, CommandDirective[A]] =
      left.parse(args, conf).catchSome { case ValidationError(CommandMismatch, _) => right.parse(args, conf) }

    lazy val synopsis: UsageSynopsis = UsageSynopsis.Mixed

    override val alternatives = List(left, right)

    def getSubcommands: Predef.Map[String, Command[_]] = left.getSubcommands ++ right.getSubcommands
  }

  final case class Subcommands[A, B](parent: Command[A], child: Command[B]) extends Command[(A, B)] with Pipeline {
    self =>

    override lazy val shortDesc = parent.shortDesc

    lazy val helpDoc = {

      def getSynopsis[C](command: Command[C], precedent: List[HelpDoc.Span]): List[(HelpDoc.Span, HelpDoc.Span)] =
        command match {
          case OrElse(left, right) =>
            getSynopsis(left, precedent) ++ getSynopsis(right, precedent)
          case Single(_, desc, _, _) =>
            val synopsisList  = precedent ++ List(command.synopsis.helpDoc.getSpan)
            val finalSynopsis = synopsisList
              .foldRight(HelpDoc.Span.empty) {
                case (HelpDoc.Span.Text(""), span) => span
                case (span, HelpDoc.Span.Text("")) => span
                case (span1, span2)                => span1 + HelpDoc.Span.text(" ") + span2
              }
            List((finalSynopsis, desc.getSpan))
          case Map(cmd, _) =>
            getSynopsis(cmd, precedent)
          case Subcommands(parent, child) =>
            val parentSynopsis = getSynopsis(parent, precedent)
            parentSynopsis.headOption match {
              case None           => getSynopsis(child, precedent)
              case Some((syn, _)) => parentSynopsis ++ getSynopsis(child, precedent ++ List(syn))
            }
        }

      def printSubcommands(subcommands: List[(HelpDoc.Span, HelpDoc.Span)]) = {
        val maxSynopsisLength = subcommands.foldRight(0) { case ((synopsis, _), max) =>
          Math.max(synopsis.size, max)
        }
        val listOfSynopsis = subcommands.map { case (syn, desc) =>
          HelpDoc.p {
            HelpDoc.Span.spans(
              syn,
              HelpDoc.Span.text(" " * (maxSynopsisLength - syn.size + 2)),
              desc
            )
          }
        }
        HelpDoc.enumeration(listOfSynopsis: _*)
      }

      parent.helpDoc + HelpDoc.h1("Commands") + printSubcommands(getSynopsis(child, List()))
    }

    lazy val names: Set[String] = parent.names

    def parse(
      args: List[String],
      conf: CliConfig
    ): IO[ValidationError, CommandDirective[(A, B)]] = {
      val helpDirectiveForChild =
        child
          .parse(args.tail, conf)
          .collect(ValidationError(ValidationErrorType.InvalidArgument, HelpDoc.empty)) {
            case CommandDirective.BuiltIn(BuiltInOption.ShowHelp(synopsis, helpDoc)) =>
              val parentName = names.headOption.getOrElse("")
              CommandDirective.builtIn {
                BuiltInOption.ShowHelp(
                  UsageSynopsis.Named(List(parentName), None) + synopsis,
                  helpDoc
                )
              }
          }

      val helpDirectiveForParent =
        ZIO.succeed(CommandDirective.builtIn(BuiltInOption.ShowHelp(synopsis, helpDoc)))

      val wizardDirectiveForChild =
        child
          .parse(args.tail, conf)
          .collect(ValidationError(ValidationErrorType.InvalidArgument, HelpDoc.empty)) {
            case directive @ CommandDirective.BuiltIn(BuiltInOption.ShowWizard(_)) => directive
          }

      val wizardDirectiveForParent =
        ZIO.succeed(CommandDirective.builtIn(BuiltInOption.ShowWizard(self)))

      parent
        .parse(args, conf)
        .flatMap {
          case CommandDirective.BuiltIn(BuiltInOption.ShowHelp(_, _)) =>
            helpDirectiveForChild orElse helpDirectiveForParent
          case CommandDirective.BuiltIn(BuiltInOption.ShowWizard(_)) =>
            wizardDirectiveForChild orElse wizardDirectiveForParent
          case builtIn @ CommandDirective.BuiltIn(_)                          => ZIO.succeed(builtIn)
          case CommandDirective.UserDefined(leftover, a) if leftover.nonEmpty =>
            child
              .parse(leftover, conf)
              .mapBoth(
                {
                  case ValidationError(CommandMismatch, _) =>
                    val parentName      = names.headOption.getOrElse("")
                    val subCommandNames = Chunk.fromIterable(getSubcommands.keys).map(n => s"'$n'")
                    val oneOf           = if (subCommandNames.size == 1) "" else " one of"
                    ValidationError(
                      CommandMismatch,
                      HelpDoc.p(
                        s"Invalid subcommand for ${parentName}. Use$oneOf ${subCommandNames.mkString(", ")}"
                      )
                    )
                  case other: ValidationError => other
                },
                _.map((a, _)).mapBuiltIn {
                  case BuiltInOption.ShowHelp(synopsis, helpDoc) =>
                    val parentName = names.headOption.getOrElse("")
                    BuiltInOption.ShowHelp(
                      UsageSynopsis.Named(List(parentName), None) + synopsis,
                      helpDoc
                    )
                  case builtIn => builtIn
                }
              )
          case _ =>
            helpDirectiveForParent
        }
        .catchSome {
          case _ if args.isEmpty =>
            helpDirectiveForParent
        }
    }

    lazy val synopsis: UsageSynopsis = parent.synopsis + child.synopsis

    def pipeline = ("", List(parent, child))

    def getSubcommands: Predef.Map[String, Command[_]] = child.getSubcommands
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
