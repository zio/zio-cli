package zio.cli

import zio.{ IO, ZIO }

import zio.cli.HelpDoc.{ h1 }
import zio.cli.HelpDoc.Span
import scala.collection.immutable.Nil

/**
 * A `Command` represents a command in a command-line application. Every command-line application
 * will have at least one command: the application itself. Other command-line applications may
 * support multiple commands.
 */
sealed trait Command[+A] { self =>
  def helpDoc: HelpDoc

  final def |[A1 >: A](that: Command[A1]): Command[A1] = Command.Fallback(self, that)

  final def as[B](b: => B): Command[B] = self.map(_ => b)

  final def map[B](f: A => B): Command[B] = Command.Map(self, f)

  def parse(args: List[String], opts: ParserOptions): IO[List[HelpDoc], (List[String], A)]

  final def subcommands[B](that: Command[B]): Command[(A, B)] = Command.Subcommands(self, that)

  final def subcommands[B](c1: Command[B], c2: Command[B], cs: Command[B]*): Command[(A, B)] =
    subcommands(cs.foldLeft(c1 | c2)(_ | _))

  def synopsis: UsageSynopsis
}

object Command {
  final case class Single[OptionsType, ArgsType](
    name: String,
    description: HelpDoc,
    options: Options[OptionsType],
    args: Args[ArgsType]
  ) extends Command[(OptionsType, ArgsType)] { self =>
    def helpDoc: HelpDoc = {
      val descriptionsSection = {
        val desc = description

        if (desc.isEmpty) HelpDoc.Empty
        else h1("description") + self.helpDoc
      }

      val argumentsSection = {
        val args = self.args.helpDoc

        if (args == HelpDoc.Empty) HelpDoc.Empty
        else h1("arguments") + self.args.helpDoc
      }

      val optionsSection = {
        val opts = self.options.helpDoc

        if (opts == HelpDoc.Empty) HelpDoc.Empty
        else h1("options") + self.options.helpDoc
      }

      descriptionsSection + argumentsSection + optionsSection
    }

    final def parse(
      args: List[String],
      opts: ParserOptions
    ): IO[List[HelpDoc], (List[String], (OptionsType, ArgsType))] = {
      type Return = IO[List[HelpDoc], (List[String], (OptionsType, ArgsType))]

      val possibilities = partitionArgs(args, opts)

      val defaultFailure =
        ZIO.fail(HelpDoc.p(Span.error(s"Expected ${self.args.minSize} arguments and options: ")) :: Nil)

      possibilities.foldLeft[Return](defaultFailure) {
        case (acc, (args, remainingArgs)) =>
          val tryCurrent =
            for {
              tuple               <- self.options.validate(args, opts)
              (args, optionsType) = tuple
              tuple               <- self.args.validate(args, opts)
              (args, argsType)    = tuple
              _ <- ZIO.when(args.nonEmpty)(
                    ZIO.fail(HelpDoc.p(Span.error(s"Unexpected arguments for command ${name}: ${args}")) :: Nil)
                  )
            } yield (remainingArgs, (optionsType, argsType))

          acc orElse tryCurrent
      }
    }

    def synopsis: UsageSynopsis =
      UsageSynopsis.Command(name) + options.synopsis + args.synopsis

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
  final case class Map[A, B](command: Command[A], f: A => B) extends Command[B] {
    def helpDoc = command.helpDoc

    final def parse(
      args: List[String],
      opts: ParserOptions
    ): IO[List[HelpDoc], (List[String], B)] = command.parse(args, opts).map {
      case (leftover, a) => (leftover, f(a))
    }

    def synopsis: UsageSynopsis = command.synopsis
  }
  final case class Fallback[A](left: Command[A], right: Command[A]) extends Command[A] {
    def helpDoc = left.helpDoc + right.helpDoc

    final def parse(
      args: List[String],
      opts: ParserOptions
    ): IO[List[HelpDoc], (List[String], A)] = left.parse(args, opts) orElse right.parse(args, opts)

    def synopsis: UsageSynopsis = UsageSynopsis.Mixed
  }
  final case class Subcommands[A, B](parent: Command[A], child: Command[B]) extends Command[(A, B)] {
    def helpDoc = parent.helpDoc + h1("subcommands") + child.helpDoc

    final def parse(
      args: List[String],
      opts: ParserOptions
    ): IO[List[HelpDoc], (List[String], (A, B))] = parent.parse(args, opts).flatMap {
      case (leftover, a) => child.parse(leftover, opts).map(t => (t._1, (a, t._2)))
    }

    def synopsis: UsageSynopsis = parent.synopsis
  }

  /**
   * Construct a new command.
   */
  def apply[OptionsType, ArgsType](
    name: String,
    options: Options[OptionsType],
    args: Args[ArgsType],
    helpDoc: HelpDoc = HelpDoc.Empty
  ): Command[(OptionsType, ArgsType)] = Single(name, helpDoc, options, args)
}
