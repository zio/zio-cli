package zio.cli

import zio._
import zio.Console._
import zio.cli.HelpDoc._

/**
 * The wizard mode: asks the user on the console for each choice, option and argument of `command`, and returns the
 * command line it builds. A [[CliApp]] starts it for `--wizard` and then runs that command line.
 *
 * At each prompt, `help` shows the full help of the current parameter, `restart` starts again from the beginning and
 * `quit` stops the wizard. An empty or invalid answer asks again.
 */
final case class Wizard(command: Command[_], conf: CliConfig, header: HelpDoc) {

  /**
   * The prompt shown before each answer: a name of `command` followed by ` :> `, such as `greet :> `.
   */
  val prompt = command.names.headOption match {
    case Some(name) => s"$name :> "
    case None       => ":> "
  }

  /**
   * Prints `header`.
   */
  val printHeader = printLine(header.toPlaintext()).orDie

  /**
   * Prints the wizard's text for `parameter`: a note that `invalidInput` was rejected (if any), then `instruction`,
   * then either the full help of `parameter` when `needHelp` is `true`, or its short description and the hints for
   * `help`, `restart` and `quit`.
   */
  def printInfo(
    invalidInput: Option[String],
    instruction: String,
    parameter: Parameter,
    needHelp: Boolean
  ): UIO[Unit] = {
    val invalid: HelpDoc = invalidInput match {
      case Some(value) => {
        val info =
          if (value == "") Span.error("No text found. ")
          else Span.error(s""""$value" was not a valid input. """)
        val error = info + Span.error(instruction)
        p(error)
      }
      case None => p(instruction)
    }
    val help: Span =
      Span.text("Write ") +
        Span.code("help") +
        Span.text(" for more information about this parameter, ") +
        Span.code("restart") +
        Span.text(" to start again Wizard mode and ") +
        Span.code("quit") +
        Span.text(" to exit.")
    val info: HelpDoc =
      if (needHelp) parameter.helpDoc
      else
        parameter.shortDesc.lastOption match {
          case Some('.') => p(Span.text(s"${parameter.shortDesc}")) + p(help)
          case Some(' ') => p(parameter.shortDesc) + p(help)
          case Some(_)   => p(Span.text(s"${parameter.shortDesc}. ")) + p(help)
          case None      => p(help)
        }
    val toPrint = h1("Wizard") + invalid + info
    printLine(toPrint.toPlaintext()).orDie
  }

  /**
   * Asks the user to pick one of the alternatives of `param`, by name or by number, and returns the words to add to the
   * command line with the chosen parameter. With a single alternative, picks it without asking.
   */
  def chooseParam(
    param: Alternatives,
    invalidInput: Option[String] = None,
    needHelp: Boolean = false
  ): IO[Wizard.WizardException, (List[String], Parameter)] = {

    val map = param.getSubparameters

    lazy val keysIndex = map.keys.zipWithIndex.toIndexedSeq

    def mkColumns(columns: List[List[String]]): HelpDoc = {

      val lengths = columns
        .map(_.map(_.length))
        .foldLeft(List.empty[Int]) { case (maxLength, textLengths) =>
          maxLength.zipAll(textLengths, 0, 0).map { case (x, y) => Math.max(x, y) }
        }

      h1("Options") + columns
        .map(_.zip(lengths).map { case (e, n) => e + (" " * (n - e.length)) }.mkString(""))
        .map(p)
        .foldLeft(HelpDoc.empty)(_ + _)
    }

    if (map.keys.size == 0) ZIO.succeed((List.empty, Command("")))
    else if (map.keys.size == 1) ZIO.succeed((map.keys.toList, map.values.head._2))
    else
      for {
        _     <- printHeader
        _     <- printInfo(invalidInput, s"Please, enter the ${param.tag} you would like to execute.", param, needHelp)
        lines <- ZIO.foreach(keysIndex) { case (key, index) =>
                   map.get(key) match {
                     case Some(param) => ZIO.succeed(List(s"[${index + 1}]", "    ", key, "  ", param._2.shortDesc))
                     case None        => ZIO.succeed(List(s"[${index + 1}]", key))
                   }
                 }
        _     <- printLine(mkColumns(lines.toList).toPlaintext()).orDie
        input <- readLine(prompt).orDie
        res   <- input match {
                 case ""        => chooseParam(param, Some(input), false)
                 case "help"    => chooseParam(param, None, true)
                 case "quit"    => ZIO.fail(Wizard.QuitException())
                 case "restart" => ZIO.fail(Wizard.RestartException())
                 case _         =>
                   map.get(input) match {
                     case Some(command) => ZIO.succeed((List(command._1), command._2))
                     case None          =>
                       {
                         for {
                           n   <- ZIO.attempt(input.toInt)
                           key <- ZIO.attempt(keysIndex(n - 1)._1)
                           res <- map.get(key) match {
                                    case Some(command) => ZIO.succeed((List(command._1), command._2))
                                    case None          => chooseParam(param, Some(input), false)
                                  }
                         } yield res
                       }.catchAll { case _ =>
                         chooseParam(param, Some(input), false)
                       }
                   }
               }
      } yield res
  }

  /**
   * Asks the user for the value of `param` until it is valid, and returns the words to add to the command line.
   */
  def inputParam(
    param: Input,
    invalidInput: Option[String] = None,
    needHelp: Boolean = false
  ): IO[Wizard.WizardException, List[String]] =
    for {
      _      <- printHeader
      _      <- printInfo(invalidInput, s"Please, specify the following ${param.tag}.", param, needHelp)
      input  <- readLine(prompt).orDie
      params <- input match {
                  case ""        => inputParam(param, Some(input), false)
                  case "help"    => inputParam(param, None, true)
                  case "quit"    => ZIO.fail(Wizard.QuitException())
                  case "restart" => ZIO.fail(Wizard.RestartException())
                  case _         =>
                    param.isValid(input, conf).catchAll { case _: ValidationError =>
                      inputParam(param, Some(input), false)
                    }
                }
    } yield params

  /**
   * Walks through `command`, asking the user for each choice, option and argument, and returns the words of the command
   * line.
   */
  def generateParams(command: Parameter): IO[Wizard.WizardException, List[String]] =
    command match {
      case p: Alternatives =>
        for {
          pair <- chooseParam(p)
          tail <- generateParams(pair._2)
        } yield pair._1 ++ tail
      case p: Input    => inputParam(p)
      case p: Pipeline =>
        for {
          pipeline <- ZIO.foreach(p.pipeline._2) { case param =>
                        generateParams(param)
                      }
        } yield pipeline.fold(List(p.pipeline._1))(_ ++ _)
      case _ => ZIO.succeed(List.empty)
    }

  /**
   * Prints the command line that the wizard built, so the user can type it directly next time.
   */
  def printFinalCommand(params: List[String]) = for {
    _ <- printHeader
    _ <- printLine(
           (p("You may bypass the wizard and execute your command directly with the following options and arguments:")
             + p("  " + params.filter(_ != "").mkString(" "))
             + p("Executing command...")).toPlaintext()
         )
  } yield ()

  /**
   * Runs the wizard and returns the command line it built, without empty words. `restart` starts again; `quit` fails
   * with `Wizard.QuitException`.
   */
  def execute: IO[Wizard.QuitException, List[String]] = for {
    parameters <- generateParams(command).catchAll {
                    case Wizard.RestartException()     => execute
                    case quit @ Wizard.QuitException() => ZIO.fail(quit)
                  }
    _ <- printFinalCommand(parameters).orDie
  } yield parameters.filter(_ != "")

}

/**
 * The exceptions that end a step of the wizard.
 */
object Wizard {

  /**
   * Raised when the user types `restart` or `quit` at a wizard prompt.
   */
  sealed trait WizardException extends Exception

  /**
   * The user typed `restart`.
   */
  case class RestartException() extends WizardException

  /**
   * The user typed `quit`.
   */
  case class QuitException() extends WizardException
}
