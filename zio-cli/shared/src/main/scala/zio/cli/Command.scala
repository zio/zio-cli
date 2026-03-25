package zio.cli

import zio.cli.HelpDoc.h1
import zio.cli.ValidationErrorType.CommandMismatch
import zio.cli.oauth2.OAuth2PlatformSpecific
import zio.{IO, ZIO}
import java.nio.file.{Paths, Files}
import scala.jdk.CollectionConverters._

sealed trait Command[+A] extends Parameter with Named { self =>
  def tag: String = ""

  // Tipi düzeltmek için map ekledik: (Unit, B) gelirse sadece B'yi döndür
  final def subcommands[B](child: Command[B]): Command[B] =
    Command.Subcommands(self, child).map(_._2)

  // Birden fazla subcommand için de aynı mantık
  final def subcommands[B](first: Command[B], rest: Command[B]*): Command[B] =
    rest.foldLeft(subcommands(first)) { (acc, next) =>
      Command.Subcommands(acc, next).map(_._2)
    }

  protected def loadOptionsFromFiles(name: String): List[String] = {
    try {
      val home = Paths.get(System.getProperty("user.home"))
      val cwd  = Paths.get(System.getProperty("user.dir"))

      def getParents(p: java.nio.file.Path, acc: List[java.nio.file.Path]): List[java.nio.file.Path] = {
        val parent = p.getParent
        if (parent == null) p :: acc
        else getParents(parent, p :: acc)
      }

      val paths = (getParents(cwd, Nil).reverse :+ home).distinct

      paths.flatMap { path =>
        val configFile = path.resolve(s".$name")
        if (Files.exists(configFile)) {
          val lines = Files.readAllLines(configFile).asScala.map(_.trim).filter(_.nonEmpty).toList
          lines.foreach(opt => println(s"Loaded option '$opt' from $configFile"))
          lines
        } else Nil
      }
    } catch { case _: Throwable => Nil }
  }

  final def |[A1 >: A](that: Command[A1]): Command[A1] = Command.OrElse(self, that)
  final def as[B](b: => B): Command[B] = map(_ => b)
  final def withHelp(help: String): Command[A] = withHelp(HelpDoc.p(help))

  final def withHelp(help: HelpDoc): Command[A] =
    self match {
      case single: Command.Single[_, _] => single.copy(help = help).asInstanceOf[Command[A]]
      case Command.Map(command, f) => Command.Map(command.withHelp(help), f)
      case Command.OrElse(left, right) => Command.OrElse(left.withHelp(help), right.withHelp(help))
      case subcommands: Command.Subcommands[_, _] => subcommands.copy(parent = subcommands.parent.withHelp(help)).asInstanceOf[Command[A]]
    }

  def getSubcommands: Predef.Map[String, Command[_]]
  def helpDoc: HelpDoc
  final def map[B](f: A => B): Command[B] = Command.Map(self, f)
  final def orElse[A1 >: A](that: Command[A1]): Command[A1] = self | that
  def parse(args: List[String], conf: CliConfig): IO[ValidationError, CommandDirective[A]]
  def synopsis: UsageSynopsis
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
  ) extends Command[(OptionsType, ArgsType)] with Pipeline with Named { self =>

    override def tag: String = "single"
    override lazy val shortDesc = help.getSpan.text

    lazy val helpDoc: HelpDoc = {
      val helpHeader = if (help.isEmpty) HelpDoc.Empty else h1("description") + help
      val argumentsSection = if (args.helpDoc == HelpDoc.Empty) HelpDoc.Empty else h1("arguments") + args.helpDoc
      val optionsSection = if (options.helpDoc == HelpDoc.Empty) HelpDoc.Empty else h1("options") + options.helpDoc
      helpHeader + argumentsSection + optionsSection + OAuth2PlatformSpecific.oauth2HelpSection(options)
    }

    lazy val names: Set[String] = Set(name)

    def parse(args: List[String], conf: CliConfig): IO[ValidationError, CommandDirective[(OptionsType, ArgsType)]] = {
      val fileOptions: List[String] = loadOptionsFromFiles(name)
      val combinedArgs: List[String] = fileOptions ++ args

      def parseBuiltInArgs(argsToParse: List[String]): IO[ValidationError, CommandDirective[Nothing]] =
        if (argsToParse.headOption.exists(conf.normalizeCase(_) == conf.normalizeCase(self.name))) {
          val opts = BuiltInOption.builtInOptions(self, self.synopsis, self.helpDoc)
          Options.validate(opts, argsToParse.tail, conf)
            .map(_._3)
            .someOrFail(ValidationError(ValidationErrorType.NoBuiltInMatch, HelpDoc.p("No built-in option matched")))
            .map(CommandDirective.BuiltIn)
        } else ZIO.fail(ValidationError(CommandMismatch, HelpDoc.p(s"Missing command name: $name")))

      val parseUserDefinedArgs =
        for {
          commandOptionsAndArgs <- combinedArgs match {
            case head :: tail if conf.normalizeCase(head.toString) == conf.normalizeCase(name) => ZIO.succeed(tail)
            case _ => ZIO.fail(ValidationError(CommandMismatch, HelpDoc.p(s"Missing command name: $name")))
          }
          parsedData = splitForcedArgs(commandOptionsAndArgs.asInstanceOf[List[String]])
          optionsAndArgs = parsedData._1
          forcedCommandArgs = parsedData._2
          tuple2 <- Options.validate(options, optionsAndArgs, conf)
          (optionsError, commandArgs, optionsType) = tuple2
          tuple <- self.args.validate(commandArgs ++ forcedCommandArgs, conf).mapError(optionsError.getOrElse(_))
          (argsLeftover, argsType) = tuple
        } yield CommandDirective.userDefined(argsLeftover, (optionsType, argsType))

      val exhaustiveSearch =
        if (combinedArgs.contains("--help") || combinedArgs.contains("-h")) parseBuiltInArgs(List(name, "--help"))
        else if (combinedArgs.contains("--wizard") || combinedArgs.contains("-w")) parseBuiltInArgs(List(name, "--wizard"))
        else ZIO.fail(ValidationError(CommandMismatch, HelpDoc.p(s"Missing command name: $name")))

      (parseBuiltInArgs(combinedArgs) orElse parseUserDefinedArgs).catchSome {
        case e: ValidationError if conf.finalCheckBuiltIn => exhaustiveSearch.catchAll(_ => ZIO.fail(e))
        case e: ValidationError => ZIO.fail(e)
      }
    }

    lazy val synopsis: UsageSynopsis = UsageSynopsis.Named(List(name), None) + options.synopsis + args.synopsis
    def pipeline = ("", List(options, args))
    def getSubcommands: Predef.Map[String, Command[_]] = Predef.Map(name -> self)
  }

  final case class Map[A, B](command: Command[A], f: A => B) extends Command[B] with Pipeline with Wrap {
    override def tag: String = "map"
    override lazy val shortDesc = command.shortDesc
    lazy val helpDoc = command.helpDoc
    lazy val names: Set[String] = command.names
    def parse(args: List[String], conf: CliConfig): IO[ValidationError, CommandDirective[B]] = command.parse(args, conf).map(_.map(f))
    lazy val synopsis: UsageSynopsis = command.synopsis
    override def wrapped: Command[A] = command
    def pipeline = ("", List(command))
    def getSubcommands: Predef.Map[String, Command[_]] = command.getSubcommands
  }

  final case class OrElse[A](left: Command[A], right: Command[A]) extends Command[A] with Alternatives {
    override def tag: String = "orElse"
    lazy val helpDoc: HelpDoc = left.helpDoc + right.helpDoc
    lazy val names: Set[String] = left.names ++ right.names
    def parse(args: List[String], conf: CliConfig): IO[ValidationError, CommandDirective[A]] =
      left.parse(args, conf).catchSome { case ValidationError(CommandMismatch, _) => right.parse(args, conf) }
    lazy val synopsis: UsageSynopsis = UsageSynopsis.Mixed
    override val alternatives = List(left, right)
    def getSubcommands: Predef.Map[String, Command[_]] = left.getSubcommands ++ right.getSubcommands
  }

  final case class Subcommands[A, B](parent: Command[A], child: Command[B]) extends Command[(A, B)] with Pipeline { self =>
    override def tag: String = "subcommands"
    override lazy val shortDesc = parent.shortDesc
    lazy val helpDoc = parent.helpDoc + HelpDoc.h1("Commands") + HelpDoc.p("Subcommands list")
    lazy val names: Set[String] = parent.names
    def parse(args: List[String], conf: CliConfig): IO[ValidationError, CommandDirective[(A, B)]] = {
      parent.parse(args, conf).flatMap {
        case CommandDirective.UserDefined(leftover, a) if leftover.nonEmpty => child.parse(leftover, conf).map(_.map((a, _)))
        case other => ZIO.succeed(other.asInstanceOf[CommandDirective[(A, B)]])
      }
    }
    lazy val synopsis: UsageSynopsis = parent.synopsis + child.synopsis
    def pipeline = ("", List(parent, child))
    def getSubcommands: Predef.Map[String, Command[_]] = child.getSubcommands
  }

  def apply[OptionsType, ArgsType](name: String, options: Options[OptionsType], args: Args[ArgsType])(implicit ev: Reducable[OptionsType, ArgsType]): Command[ev.Out] =
    Single(name, HelpDoc.empty, options, args).map(ev.fromTuple2(_))

  def apply[OptionsType](name: String, options: Options[OptionsType])(implicit ev: Reducable[OptionsType, Unit]): Command[ev.Out] =
    Single(name, HelpDoc.empty, options, Args.none).map(ev.fromTuple2(_))

  def apply[ArgsType](name: String, args: Args[ArgsType])(implicit ev: Reducable[Unit, ArgsType]): Command[ev.Out] =
    Single(name, HelpDoc.empty, Options.none, args).map(ev.fromTuple2(_))

  def apply(name: String)(implicit ev: Reducable[Unit, Unit]): Command[ev.Out] =
    Single(name, HelpDoc.empty, Options.none, Args.none).map(ev.fromTuple2(_))
}