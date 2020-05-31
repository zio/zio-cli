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
sealed trait Command[-R, +E] { self =>
  type OptionsType
  type ArgsType
  def action: String
  def options: Options[OptionsType]
  def args: Args[ArgsType]
  def execute(a: OptionsType, b: ArgsType): ZIO[R, E, Any]
  def children: List[Command[R, E]]

  def options[A1](options0: Options[A1]): Command.Aux[R, E, (self.OptionsType, A1), self.ArgsType] = new Command[R, E] {
    override type OptionsType = (self.OptionsType, A1)
    override type ArgsType    = self.ArgsType

    override def action: String = self.action

    override def options: Options[OptionsType] = self.options :: options0

    override def args: Args[ArgsType] = self.args

    override def execute(
      a: OptionsType,
      b: ArgsType
    ): ZIO[R, E, Any] = self.execute(a._1, b)

    override def children: List[Command[R, E]] = self.children
  }

  def args[B1](args0: Args[B1]): Command.Aux[R, E, self.OptionsType, (self.ArgsType, B1)] = new Command[R, E] {
    override type OptionsType = self.OptionsType
    override type ArgsType    = (self.ArgsType, B1)

    override def action: String = self.action

    override def options: Options[OptionsType] = self.options

    override def args: Args[ArgsType] = self.args :: args0

    override def execute(
      a: OptionsType,
      b: ArgsType
    ): ZIO[R, E, Any] = self.execute(a, b._1)

    override def children: List[Command[R, E]] = self.children
  }

  def execute[R1 <: R, E1 >: E](
    f: (OptionsType, ArgsType) => ZIO[R1, E1, Any]
  ): Command.Aux[R1, E1, self.OptionsType, self.ArgsType] =
    new Command[R1, E1] {
      override type OptionsType = self.OptionsType
      override type ArgsType    = self.ArgsType

      override def action: String = self.action

      override def options: Options[OptionsType] = self.options

      override def args: Args[ArgsType] = self.args

      override def execute(
        a: OptionsType,
        b: ArgsType
      ): ZIO[R1, E1, Any] = self.execute(a, b) *> f(a, b)

      override def children: List[Command[R, E]] = self.children
    }

  def children[R1 <: R, E1 >: E](
    children0: List[Command[R1, E1]]
  ): Command.Aux[R1, E1, self.OptionsType, self.ArgsType] =
    new Command[R1, E1] {
      override type OptionsType = self.OptionsType
      override type ArgsType    = self.ArgsType

      override def action: String = self.action

      override def options: Options[OptionsType] = self.options

      override def args: Args[ArgsType] = self.args

      override def execute(
        a: OptionsType,
        b: ArgsType
      ): ZIO[R1, E1, Any] = self.execute(a, b)

      override def children: List[Command[R1, E1]] = self.children ++ children0
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
  type Aux[-R0, +E0, OptionsType0, ArgsType0] = Command[R0, E0] {
    type OptionsType = OptionsType0
    type ArgsType    = ArgsType0
  }

  /**
   * Construct a new command with the specified command name.
   */
  def apply(
    action0: String
  ): Command.Aux[Any, Nothing, Any, Any] = new Command[Any, Nothing] {
    override type OptionsType = Any
    override type ArgsType    = Any

    override def action: String = action0

    override def options: Options[OptionsType] = Options.Empty

    override def args: Args[ArgsType] = Args.Empty

    override def execute(a: OptionsType, b: ArgsType): ZIO[Any, Nothing, Any] = ZIO.unit

    override def children: List[Command[Any, Nothing]] = Nil
  }

}
