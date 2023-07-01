package zio.cli

import zio._
import zio.cli.Options._
import zio.cli.HelpDoc._

final case class Wizardd(command: Command[_]) {

    val header = h1("WIZARD")
    val explanation = p("Wizard mode helps you constructing commands") + p("asd")
    val prompt = command.names.headOption match {
        case Some(name) => h1(s"$name :> ").toPlaintext()
        case None => h1(":> ").toPlaintext()
    }
    val printHeader = Console.printLine((header + explanation).toPlaintext()).orDie

    def printInvalid(invalidInput: String): UIO[Option[Unit]] =
        ZIO.when(invalidInput != "") {
            Console.printLine(s"$invalidInput was not a valid input. Try again.").orDie
        }

    case class InvalidValue() extends Throwable with Serializable

    def checkArgs[A](args: Args.Single[A], input: String): IO[InvalidValue, List[String]] =
        for {
            _ <- ZIO.unit
            _ <- args.validate(List(input), CliConfig.default).catchAll{
                case _: ValidationError => ZIO.fail(InvalidValue())
                case _ => ZIO.fail(InvalidValue())
            }
        } yield List(input)

    def inputArgs[A](args: Args.Single[A], invalidInput: String = ""): UIO[List[String]] =
        for {
            _ <- printInvalid(invalidInput)
            _ <- printHeader
            input <- Console.readLine(prompt).orDie
            args <- checkArgs(args, input).catchSome {
                case _: InvalidValue => inputArgs(args, input)
            }.orDie
        } yield args     

    def traverseArgs[A](args: Args[A]): UIO[List[String]] =
        args match {
            case Args.Empty => ZIO.succeed(List.empty)
            case Args.Both(left, right) => traverseArgs(left).zipWith(traverseArgs(right))(_ ++ _)
            case Args.Map(values, _) => traverseArgs(values)
            case Args.Variadic(args, min, max) =>
                (min, max) match {
                    case (Some(min), Some(max)) if 0 >= min && 0 < max => for {
                        repeat <- repeat
                        res <- 
                            if(repeat) traverseArgs(args).zipWith(traverseArgs(Args.Variadic(args, Some(min - 1), Some(max - 1))))(_ ++ _)
                            else ZIO.succeed(List.empty)
                    } yield res
                    case (Some(min), _) if 0 < min => traverseArgs(args).zipWith(traverseArgs(Args.Variadic(args, Some(min - 1), max.map(_ - 1))))(_ ++ _)
                    case (_, Some(max)) if max <= 0 => ZIO.succeed(List.empty)
                    case _                      => for {
                        repeat <- repeat
                        res <- 
                            if(repeat) traverseArgs(args).zipWith(traverseArgs(Args.Variadic(args, min.map(_ - 1), max.map(_ - 1))))(_ ++ _)
                            else ZIO.succeed(List.empty)
                    } yield res
                }
            case _ => traverseArgs(args)

        }

    val repeat: UIO[Boolean] = for {
        _ <- Console.printLine(s"Do you wish to continue entering arguments ...? (Y/N)").orDie
        response <- Console.readLine.orDie
        res <- 
            if (response.toLowerCase == "y") ZIO.succeed(true)
            else if(response.toLowerCase == "n") ZIO.succeed(false)
            else repeat
    } yield res

    /**
     * Methods for Options
     */

    /**
     * Checks that the input is valid
     */
    def checkOptions[A](options: Options[A], input: String): IO[InvalidValue, List[String]] =
        (options match {
                case KeyValueMap(argumentOption) => 
                    ZIO.succeed(options.uid.getOrElse("") :: input.split(" ").toList)
                case opt@WithDefault(options, default) => ZIO.succeed(
                    if (options.isBool) {
                        if (PrimType.Bool.TrueValues.contains(input)) List(options.uid.getOrElse("")) else List.empty
                    } else {
                        if (input.isEmpty) List.empty else List(options.uid.getOrElse(""), input)
                    })
                case OrElse(left, right) => 
                    if (input == left.uid.getOrElse("")) traverseOptions(left)
                    else if (input == right.uid.getOrElse("")) traverseOptions(right)
                    else ZIO.fail(InvalidValue())
                case _: OAuth2Options => ???
                case opt@Single(name, aliases, primType, description) => 
                    for {
                        _ <- ZIO.unit
                        //_ <- opt.validate(List(opt.names.head, input), CliConfig.default).orDie
                    } yield List(opt.names.head, input)
                case _ => traverseOptions(options)
            })

    /**
     * Pipeline to get an input for an option from user
     */
    def inputOptions[A](options: Options[A], invalidInput: String = ""): UIO[List[String]] =
        for {
            _ <- printInvalid(invalidInput)
            _ <- printHeader
            input <- Console.readLine(prompt).orDie
            options <- checkOptions(options, input).catchSome {
                case _: InvalidValue => inputOptions(options, input)
            }.orDie
        } yield options


        

    def traverseOptions[A](options: Options[A]): UIO[List[String]] =
        options match {
            case Options.Empty => ZIO.succeed(List.empty)
            case Both(left, right) => traverseOptions(left).zipWith(traverseOptions(right))(_ ++ _)
            case Options.Map(values, _) => traverseOptions(values)
            case _ => inputOptions(options)
        }

    def getCommand(command: Command[_], invalidInput: String = ""): UIO[(List[String], Command[_])] = {
        val subcommands = command.getSubcommands
        if(subcommands.keys.size == 0) ZIO.succeed((List.empty, Command("")))
        else if(subcommands.keys.size == 1) ZIO.succeed((subcommands.keys.toList, subcommands.values.head))
        else for {
            _ <- printInvalid(invalidInput)
            _ <- printHeader
            lines <- ZIO.foreach(subcommands.keys.zipWithIndex){ case (key, index) =>
                subcommands.get(key) match {
                    case Some(Command.Single(_, desc, _, _)) => ZIO.succeed(List(s"[$index]", "    ", key, "  ", desc.toPlaintext()))
                    case Some(_) => ZIO.succeed(List(s"[$index]", key))
                    case None => ZIO.succeed(List(s"[$index]", key))
                }
            }
            _ <- ZIO.foreach(mkColumns(lines.toList)) { case line =>
                Console.printLine(line).orDie
            }
            input <- Console.readLine(prompt).orDie
            res <- subcommands.get(input) match {
                case Some(command) => ZIO.succeed((List(input), command))
                case None => getCommand(command, input)
            }
        } yield res
    }

    def mkColumns(columns: List[List[String]]): List[String] = {

        def max(l1: List[Int], l2: List[Int]): List[Int] =
            (l1, l2) match {
                case (Nil, l2) => l2
                case (l1, Nil) => l1
                case (e1 :: l1, e2 :: l2) => Math.max(e1, e2) :: max(l1, l2)
            }

        def mk(l: List[String], len: List[Int]): List[String] =
            (l, len) match {
                case (Nil, len) => List()
                case (l, Nil) => List()
                case (e :: l, n :: len) => (e + (" " * (n - e.length))) :: mk(l, len)  
            }

    
        val lengths = columns.map(_.map(_.length)).foldLeft(List.empty[Int]) {
            case (maxLength, textLengths) => max(maxLength, textLengths)
        }

        columns.map(mk(_, lengths).mkString(""))
    }   

    def generateParams(command: Command[_]): UIO[List[String]] =  
      command match {
        case Command.Single(name, help, options, args) => for {
            optionsList <- traverseOptions(options)
            argsList <- traverseArgs(args)
        } yield name :: optionsList ++ argsList
        case Command.Map(command, f) => generateParams(command)
        case c@Command.OrElse(left, right) => for {
            nextCommand <- getCommand(c)
            params <- generateParams(nextCommand._2)
        } yield nextCommand._1 ++ params
        case Command.Subcommands(parent, child) => generateParams(parent).zipWith(generateParams(child))(_ ++ _)
      }

    def printFinalCommand(params: List[String]) =
        Console.printLine("You may bypass the wizard and execute your command directly with the following options and arguments:") *>
            Console.printLine("  " + params.mkString(" "))

}

object StringUtil extends ZIOCliDefault {
    import zio.cli.HelpDoc.Span.text
import zio.cli.HelpDoc.p

  sealed trait Subcommand
  object Subcommand {
    final case class Split(string: String, first: Boolean, separator: String) extends Subcommand
    final case class Join(strings: NonEmptyChunk[String], separator: String)  extends Subcommand
  }

  val firstOption =
    Options.boolean(name = "first").alias("f") ?? "Display just the first substring."
  val separatorOption = Options.text("separator").alias("s").withDefault(",") ?? "Separator regex."
  val stringArg       = Args.text("string") ?? "String to split."

  val split =
    Command("split", firstOption ++ separatorOption, stringArg)
      .withHelp(p("Split a string into substrings and display as an array"))
      .map { case ((first, separator), string) =>
        Subcommand.Split(string, first, separator)
      }

  val join =
    Command("join", separatorOption, Args.text("string").+ ?? "Strings to join.")
      .withHelp(p("Join the command-arguments into a single string"))
      .map { case (separator, strings) =>
        Subcommand.Join(NonEmptyChunk.fromCons(strings), separator)
      }

  val stringUtil: Command[Subcommand] =
    Command("string-util", Options.none, Args.none).subcommands(split, join)

  val cliApp = CliApp.make(
    name = "String Util",
    version = "0.0.1",
    summary = text("CLI to some string utilities"),
    footer = HelpDoc.p("©Copyright 2022"),
    command = stringUtil
  ) {
    case Subcommand.Split(string, first, separator) =>
      val elements = string.split(separator)
      Console.printLine(if (first) elements.headOption.getOrElse("") else elements.mkString("[", ", ", "]"))
    case Subcommand.Join(strings, separator) =>
      Console.printLine(strings.mkString(separator))
  }
}

object T extends ZIOAppDefault {
  override val run = StringUtil.cliApp.run(List("--wizard"))

}
