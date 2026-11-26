package zio.cli

import zio.cli.HelpDoc.h1
import zio.cli.ValidationErrorType.CommandMismatch
import zio.cli.oauth2.OAuth2PlatformSpecific
import zio.{Chunk, IO, ZIO}

/**
 * A `Command[A]` describes a command of a command-line application: its name, its [[Options]], its [[Args]], its help
 * text and, if it has any, its subcommands. Parsing a command line with it produces a value of type `A`.
 *
 * Create one with `Command(name, options, args)`, add a description with `withHelp`, offer several commands with `|`,
 * and nest commands with `subcommands`. A [[CliApp]] runs a command.
 *
 * {{{
 * val verbose = Options.boolean("verbose").alias("v")
 * val file    = Args.text("file")
 *
 * val grep: Command[(Boolean, String)] = Command("grep", verbose, file).withHelp("Searches a file.")
 * }}}
 *
 * @see
 *   [[https://zio.dev/zio-cli/commands/ Commands]]
 */
sealed trait Command[+A] extends Parameter with Named { self =>

  /**
   * Offers `that` as an alternative: parsing tries this command first and, when the first word isn't its name, tries
   * `that`. When no command matches, the error names the last one tried (`Missing command name: cat`).
   */
  final def |[A1 >: A](that: Command[A1]): Command[A1] = Command.OrElse(self, that)

  /**
   * Replaces the parsed value with `b`.
   */
  final def as[B](b: => B): Command[B] = map(_ => b)

  /**
   * Sets the description shown in the description section of this command's help, as one paragraph, replacing any
   * earlier one. On alternatives (`a | b`), every command gets the description; on a command with subcommands, the
   * parent gets it.
   */
  final def withHelp(help: String): Command[A] =
    withHelp(HelpDoc.p(help))

  /**
   * Sets the description shown in the description section of this command's help, replacing any earlier one. On
   * alternatives (`a | b`), every command gets the description; on a command with subcommands, the parent gets it.
   */
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

  /**
   * Returns the commands a user can pick at this level, by name: the command itself for a single command, every
   * alternative for `a | b`, and the subcommands for a command with subcommands.
   */
  def getSubcommands: Map[String, Command[_]]

  /**
   * Returns the help text of this command: the description, arguments and options sections (each left out when empty)
   * and, for a command with subcommands, a commands section that lists them.
   */
  def helpDoc: HelpDoc

  /**
   * Transforms the parsed value with `f`.
   */
  final def map[B](f: A => B): Command[B] = Command.Map(self, f)

  /**
   * Same as `|`.
   */
  final def orElse[A1 >: A](that: Command[A1]): Command[A1] = self | that

  /**
   * Offers `that` as an alternative, and returns `Left` with the value of this command or `Right` with the value of
   * `that`.
   */
  final def orElseEither[B](that: Command[B]): Command[Either[A, B]] = map(Left(_)) | that.map(Right(_))

  /**
   * Parses `args`, whose first word must be the command's name, and returns a [[CommandDirective]]: `UserDefined` with
   * the parsed value and the words left over, or `BuiltIn` when the user asked for help, the wizard or shell
   * completion. The options are read first, then the arguments; words after `--` are always read as arguments. When the
   * first word isn't the command's name, it fails with a [[ValidationError]] of type `CommandMismatch` and the text
   * `Missing command name: grep`. For a single command or a command with subcommands, [[CliApp]] adds the name in front
   * of the user's words before it calls this method.
   */
  def parse(args: List[String], conf: CliConfig): IO[ValidationError, CommandDirective[A]]

  /**
   * Adds `that` as the subcommand of this command: after this command's options, the next word picks the subcommand.
   * The value pairs this command's value with the subcommand's, with a `Unit` side dropped. With no subcommand given,
   * parsing returns this command's help. An unknown subcommand fails with
   * `Invalid subcommand for git. Use one of 'add', 'rm'`.
   *
   * {{{
   * val add = Command("add", Args.text("path"))
   * val rm  = Command("rm", Args.text("path"))
   *
   * val git: Command[String] = Command("git").subcommands(add | rm)
   * }}}
   */
  final def subcommands[B](that: Command[B])(implicit ev: Reducable[A, B]): Command[ev.Out] =
    Command.Subcommands(self, that).map(ev.fromTuple2(_))

  /**
   * Adds `c1`, `c2` and `cs` as the subcommands of this command, as `subcommands(c1 | c2 | ...)` does. All of them have
   * the same value type.
   */
  final def subcommands[B](c1: Command[B], c2: Command[B], cs: Command[B]*)(implicit
    ev: Reducable[A, B]
  ): Command[ev.Out] =
    subcommands(cs.foldLeft(c1 | c2)(_ | _))(ev)

  /**
   * Returns the usage synopsis of this command, such as `grep [(-v, --verbose)] <file>`.
   */
  def synopsis: UsageSynopsis

  /**
   * The word `command`, which the wizard mode uses to name this kind of parameter.
   */
  lazy val tag: String = "command"
}

/**
 * Constructors for [[Command]], and the cases a command is built from.
 */
object Command {

  private def splitForcedArgs(args: List[String]): (List[String], List[String]) = {
    val (remainingArgs, forcedArgs) = args.span(_ != "--")
    (remainingArgs, forcedArgs.drop(1))
  }

  /**
   * A command with a `name`, a `help` description, its `options` and its `args`. Its value is the pair of the options'
   * value and the arguments' value; `Command.apply` drops a `Unit` side.
   */
  final case class Single[OptionsType, ArgsType](
    name: String,
    help: HelpDoc,
    options: Options[OptionsType],
    args: Args[ArgsType]
  ) extends Command[(OptionsType, ArgsType)]
      with Pipeline
      with Named { self =>

    override lazy val shortDesc = help.getSpan.text

    /**
     * The description, arguments and options sections, each left out when empty, followed by a section on third-party
     * authorization when the options include an option built by `Options.oauth2`.
     */
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

    /**
     * A set with `name`.
     */
    lazy val names: Set[String] = Set(name)

    /**
     * Checks that the first word is `name` (ignoring case unless [[CliConfig]] is case-sensitive), then reads the
     * options and the arguments. When the words after the name are only built-in options (`--help`, `-h`, `--wizard` or
     * the shell-completion options), it returns a `BuiltIn` directive. When parsing fails and
     * `CliConfig.finalCheckBuiltIn` is on, `--help`, `-h`, `--wizard` or `-w` anywhere in `args` give the help or the
     * wizard instead of the error, with help first when both are present.
     */
    def parse(
      args: List[String],
      conf: CliConfig
    ): IO[ValidationError, CommandDirective[(OptionsType, ArgsType)]] = {
      def parseBuiltInArgs(args: List[String]): IO[ValidationError, CommandDirective[Nothing]] =
        if (args.headOption.exists(conf.normalizeCase(_) == conf.normalizeCase(self.name))) {
          val options = BuiltInOption
            .builtInOptions(self, self.synopsis, self.helpDoc)
          def noBuiltInMatch =
            ValidationError(ValidationErrorType.NoBuiltInMatch, HelpDoc.p("No built-in option was matched"))
          Options
            .validate(options, args.tail, conf)
            .collect(noBuiltInMatch) { case (_, Nil, Some(value)) => CommandDirective.BuiltIn(value) }
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

    /**
     * The name followed by the synopses of the options and the arguments.
     */
    lazy val synopsis: UsageSynopsis =
      UsageSynopsis.Named(List(name), None) + options.synopsis + args.synopsis

    /**
     * The options and the arguments, which the wizard mode asks for in this order.
     */
    def pipeline = ("", List(options, args))

    /**
     * A map from `name` to this command.
     */
    def getSubcommands: Predef.Map[String, Command[_]] = Predef.Map(name -> self)
  }

  /**
   * A command whose value is transformed by `f`. `map`, `as` and `Command.apply` build it.
   */
  final case class Map[A, B](command: Command[A], f: A => B) extends Command[B] with Pipeline with Wrap {

    override lazy val shortDesc = command.shortDesc

    /**
     * The help of `command`.
     */
    lazy val helpDoc = command.helpDoc

    /**
     * The names of `command`.
     */
    lazy val names: Set[String] = command.names

    /**
     * Parses with `command`, and applies `f` to the value of a `UserDefined` directive.
     */
    def parse(
      args: List[String],
      conf: CliConfig
    ): IO[ValidationError, CommandDirective[B]] =
      command.parse(args, conf).map(_.map(f))

    /**
     * The synopsis of `command`.
     */
    lazy val synopsis: UsageSynopsis = command.synopsis

    override def wrapped: Command[A] = command

    /**
     * The wrapped `command`, for the wizard mode.
     */
    def pipeline = ("", List(command))

    /**
     * The subcommands of `command`.
     */
    def getSubcommands: Predef.Map[String, Command[_]] = command.getSubcommands
  }

  /**
   * Two alternative commands. `|`, `orElse` and `orElseEither` build it.
   */
  final case class OrElse[A](left: Command[A], right: Command[A]) extends Command[A] with Alternatives {

    /**
     * The help of `left` followed by the help of `right`.
     */
    lazy val helpDoc: HelpDoc = left.helpDoc + right.helpDoc

    /**
     * The names of both commands.
     */
    lazy val names: Set[String] = left.names ++ right.names

    /**
     * Parses with `left`; when that fails with `CommandMismatch` (the first word isn't its name), parses with `right`.
     */
    def parse(
      args: List[String],
      conf: CliConfig
    ): IO[ValidationError, CommandDirective[A]] =
      left.parse(args, conf).catchSome { case ValidationError(CommandMismatch, _) => right.parse(args, conf) }

    /**
     * Always `UsageSynopsis.Mixed`, shown as `<command>`.
     */
    lazy val synopsis: UsageSynopsis = UsageSynopsis.Mixed

    override val alternatives = List(left, right)

    /**
     * The subcommands of both commands.
     */
    def getSubcommands: Predef.Map[String, Command[_]] = left.getSubcommands ++ right.getSubcommands
  }

  /**
   * A `parent` command followed by a `child` command chosen by name. `subcommands` builds it.
   */
  final case class Subcommands[A, B](parent: Command[A], child: Command[B]) extends Command[(A, B)] with Pipeline {
    self =>

    override lazy val shortDesc = parent.shortDesc

    /**
     * The help of `parent` followed by a commands section with one line per subcommand: its usage and, when its
     * description is a single paragraph, that paragraph.
     */
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

    /**
     * The names of `parent`.
     */
    lazy val names: Set[String] = parent.names

    /**
     * Parses `parent`, then picks and parses the child from the words `parent` leaves. With no word left, returns the
     * help of the parent. `--help` after a subcommand's name gives that subcommand's help, with the parent's name in
     * front of its usage line.
     */
    def parse(
      args: List[String],
      conf: CliConfig
    ): IO[ValidationError, CommandDirective[(A, B)]] = {
      val childConf       = conf.copy(finalCheckBuiltIn = false)
      def invalidArgument = ValidationError(ValidationErrorType.InvalidArgument, HelpDoc.empty)

      val helpDirectiveForChild =
        if (args.isEmpty)
          ZIO.fail(invalidArgument)
        else
          child
            .parse(args.tail, childConf)
            .collect(invalidArgument) { case CommandDirective.BuiltIn(BuiltInOption.ShowHelp(synopsis, helpDoc)) =>
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
        if (args.isEmpty)
          ZIO.fail(invalidArgument)
        else
          child
            .parse(args.tail, childConf)
            .collect(invalidArgument) { case directive @ CommandDirective.BuiltIn(BuiltInOption.ShowWizard(_)) =>
              directive
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
              .parse(leftover, childConf)
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

    /**
     * The synopsis of `parent` followed by the synopsis of `child`.
     */
    lazy val synopsis: UsageSynopsis = parent.synopsis + child.synopsis

    /**
     * The parent and the child, which the wizard mode asks for in this order.
     */
    def pipeline = ("", List(parent, child))

    /**
     * The subcommands of `child`, by name.
     */
    def getSubcommands: Predef.Map[String, Command[_]] = child.getSubcommands
  }

  /**
   * Creates a command named `name` with `options` and `args`. Its value is the pair of their values, with a `Unit` side
   * dropped.
   */
  def apply[OptionsType, ArgsType](
    name: String,
    options: Options[OptionsType],
    args: Args[ArgsType]
  )(implicit ev: Reducable[OptionsType, ArgsType]): Command[ev.Out] =
    Single(name, HelpDoc.empty, options, args).map(ev.fromTuple2(_))

  /**
   * Creates a command named `name` with `options` and no arguments. Its value is the options' value.
   */
  def apply[OptionsType](
    name: String,
    options: Options[OptionsType]
  )(implicit ev: Reducable[OptionsType, Unit]): Command[ev.Out] =
    Single(name, HelpDoc.empty, options, Args.none).map(ev.fromTuple2(_))

  /**
   * Creates a command named `name` with `args` and no options. Its value is the arguments' value.
   */
  def apply[ArgsType](
    name: String,
    args: Args[ArgsType]
  )(implicit ev: Reducable[Unit, ArgsType]): Command[ev.Out] =
    Single(name, HelpDoc.empty, Options.none, args).map(ev.fromTuple2(_))

  /**
   * Creates a command named `name` with no options and no arguments. Its value is `()`.
   */
  def apply(
    name: String
  )(implicit ev: Reducable[Unit, Unit]): Command[ev.Out] =
    Single(name, HelpDoc.empty, Options.none, Args.none).map(ev.fromTuple2(_))
}
