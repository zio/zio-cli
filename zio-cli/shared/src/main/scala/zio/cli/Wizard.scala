package zio.cli

import zio._
import zio.cli.HelpDoc._

final case class Wizard(command: Command[_], conf: CliConfig, header: HelpDoc) {

    
    val prompt = command.names.headOption match {
        case Some(name) => s"$name :> "
        case None => ":> "
    }
    val printHeader = Console.printLine(header.toPlaintext()).orDie

    def printInvalid(invalidInput: String): UIO[Option[Unit]] =
        ZIO.when(invalidInput != "") {
            Console.printLine(s"$invalidInput was not a valid input. Try again.").orDie
        }

    def mkColumns(columns: List[List[String]]): HelpDoc = {

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

        h1("Choose between the following:") +
            columns.map(mk(_, lengths).mkString("")).map(p).foldLeft(HelpDoc.empty)(_ + _)
    }   

    def getParam(map: Predef.Map[String, (String, Parameter)], invalidInput: String = ""): UIO[(List[String], Parameter)] = {
        if(map.keys.size == 0)  ZIO.succeed((List.empty, Command("")))
        else if(map.keys.size == 1)  ZIO.succeed((map.keys.toList, map.values.head._2))
        else for {
            _ <- printHeader
            _ <- printInvalid(invalidInput)
            keysIndex = map.keys.zipWithIndex.toIndexedSeq
            lines <- ZIO.foreach(keysIndex){ case (key, index) =>
                map.get(key) match {
                    case Some(param) => ZIO.succeed(List(s"[${index + 1}]", "    ", key, "  ", param._2.shortDesc.toPlaintext()))
                    case None => ZIO.succeed(List(s"[${index + 1}]", key))
                }
            }
            _ <- Console.printLine(mkColumns(lines.toList).toPlaintext()).orDie
            input <- Console.readLine(prompt).orDie
            res <- map.get(input) match {
                case Some(command) => ZIO.succeed((List(command._1), command._2))
                case None => {for {
                    n <- ZIO.attempt(input.toInt)
                    key <- ZIO.attempt(keysIndex(n - 1)._1)
                    res <- map.get(key) match {
                        case Some(command) => ZIO.succeed((List(command._1), command._2))
                        case None => getParam(map, input)
                    }
                } yield res }.catchAll {
                    case _ => getParam(map, input)
                }
            }
        } yield res
    }

    def ask(isValid: ((String, CliConfig) => IO[ValidationError, List[String]]), info: HelpDoc, invalidInput: String = ""): UIO[List[String]] =
        for {
            _ <- printInvalid(invalidInput)
            _ <- printHeader
            _ <- Console.printLine(info.toPlaintext()).orDie
            input <- Console.readLine(prompt).orDie
            params <- isValid(input, conf).catchAll {
                case _: ValidationError => ask(isValid, info, input)
            }
        } yield params


    def generateParams(command: Parameter): UIO[List[String]] =
        command match {
            case p: Sub => for {
                pair <- getParam(p.getSubparameters)
                tail <- generateParams(pair._2)
            } yield pair._1 ++ tail
            case p: Validable => ask(p.isValid, p.wizardInfo)
            case p: Lista => for {
                lista <- ZIO.foreach(p.lista._2) {
                    case param => generateParams(param)
                }
            } yield lista.fold(Nil)(_ ++ _)
            case _ => ZIO.succeed(List.empty)
        }

    def execute: UIO[List[String]] = for {
        parameters <- generateParams(command)
        _ <- printFinalCommand(parameters).orDie
    } yield parameters
      

    def printFinalCommand(params: List[String]) = for {
        _ <- printHeader
        _ <- Console.printLine("You may bypass the wizard and execute your command directly with the following options and arguments:")
        _ <- Console.printLine("  " + params.mkString(" "))
    } yield ()
            

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
