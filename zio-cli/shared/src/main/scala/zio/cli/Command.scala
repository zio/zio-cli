package zio.cli

import zio.{ IO, ZIO }

import zio.cli.HelpDoc.dsl._
import scala.collection.immutable.Nil

/**
 * OptionsType `Command[R, E]` is a model of a command that can be given to a command-
 * line application.
 *
 * Commands may have children, which represent subcommands.
 */
sealed trait Command[+A] { self =>
  type OptionsType
  type ArgsType

  def action: String
  def options: Options[OptionsType]
  def args: Args[ArgsType]
  def output(options: OptionsType, args: ArgsType): A

  def args[B1](args0: Args[B1])(implicit ev: Any <:< ArgsType): Command.Aux[self.OptionsType, B1, (self.OptionsType, B1)] =
    new Command[(self.OptionsType, B1)] {
      override type OptionsType = self.OptionsType
      override type ArgsType    = B1

      override def action: String = self.action

      override def options: Options[OptionsType] = self.options

      override def args: Args[ArgsType] = args0

      def output(options: OptionsType, args: ArgsType): (self.OptionsType, B1) = (options, args)
    }

  def as[B](b: => B): Command.Aux[OptionsType, ArgsType, B] = self.map(_ => b)

  def map[B](f: A => B): Command.Aux[OptionsType, ArgsType, B] =
    new Command[B] {
      override type OptionsType = self.OptionsType
      override type ArgsType    = self.ArgsType

      override def action: String = self.action

      override def options: Options[OptionsType] = self.options

      override def args: Args[ArgsType] = self.args

      def output(options: OptionsType, args: ArgsType): B = f(self.output(options, args))
    }

  def options[A1](
    options0: Options[A1]
  )(implicit ev: Any <:< OptionsType): Command.Aux[A1, self.ArgsType, (A1, self.ArgsType)] =
    new Command[(A1, self.ArgsType)] {
      override type OptionsType = A1
      override type ArgsType    = self.ArgsType

      override def action: String = self.action

      override def options: Options[OptionsType] = options0

      override def args: Args[ArgsType] = self.args

      def output(options: OptionsType, args: ArgsType): (A1, self.ArgsType) = (options, args)
    }

  /**
   * Generates the help doc for this command, and any subcommands.
   */
  final def helpDoc: HelpDoc.Block = ???

  /**
   * Validates the arguments from the command line, either returning a failure
   * that includes detailed documentation, or returning a tuple that contains
   * both options and arguments, together with remaining (unparsed) arguments
   * from the command-line.
   */
  final def validate(
    args: List[String],
    opts: ParserOptions
  ): IO[List[HelpDoc.Block], (List[String], OptionsType, ArgsType)] = {
    type Return = IO[List[HelpDoc.Block], (List[String], OptionsType, ArgsType)]

    val possibilities = partitionArgs(args, opts)

    val defaultFailure = ZIO.fail(p(error(s"Expected ${self.args.minSize} arguments and options: ")) :: Nil)

    possibilities.foldLeft[Return](defaultFailure) {
      case (acc, (args, remainingArgs)) =>
        val tryCurrent =
          for {
            tuple               <- self.options.validate(args, opts)
            (args, optionsType) = tuple
            tuple               <- self.args.validate(args, opts)
            (args, argsType)    = tuple
            _ <- ZIO.when(args.nonEmpty)(
                  ZIO.fail(p(error(s"Unexpected arguments for command ${action}: ${args}")) :: Nil)
                )
          } yield (remainingArgs, optionsType, argsType)

        acc orElse tryCurrent
    }
  }

  private def partitionArgs(args: List[String], opts: ParserOptions): Set[(List[String], List[String])] = {
    def loop(argsMatched: Int, args: List[String], opts: ParserOptions): Set[(List[String], List[String])] =
      args match {
        case head :: tail =>
          self.options.recognizes(head, opts) match {
            case Some(value) =>
              val ourArgs = head :: tail.take(value)
              val unknown = tail.drop(value)

              loop(argsMatched, unknown, opts).map {
                case (ourArgs2, theirArgs) =>
                  (ourArgs ++ ourArgs2, theirArgs)
              }

            case None =>
              val minSize = self.args.minSize
              val maxSize = self.args.maxSize

              val option1 =
                if (argsMatched < maxSize) {
                  loop(argsMatched + 1, tail, opts).map {
                    case (ourArgs, theirArgs) => (head :: ourArgs, theirArgs)
                  }
                } else Set()

              val option2 =
                if (argsMatched >= minSize) Set((Nil, tail))
                else Set()

              option1 ++ option2
          }

        case Nil => Set((Nil, Nil))
      }

    loop(0, args, opts)
  }
}

object Command {
  type Aux[OptionsType0, ArgsType0, +Out] = Command[Out] {
    type OptionsType = OptionsType0
    type ArgsType    = ArgsType0
  }

  /**
   * Construct a new command with the specified command name.
   */
  def apply(
    action0: String
  ): Command.Aux[Any, Any, Unit] = new Command[Unit] {
    override type OptionsType = Any
    override type ArgsType    = Any

    override def action: String = action0

    override def options: Options[OptionsType] = Options.Empty

    override def args: Args[ArgsType] = Args.Empty

    def output(options: OptionsType, args: ArgsType): Unit = ()
  }
}
