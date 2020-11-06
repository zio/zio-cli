package zio.cli

import java.nio.file.{Path => JPath}

import zio.IO
import zio.cli.HelpDoc.dsl

sealed trait Args[+A] { self =>

  def ++[That, A1 >: A](that: Args[That]): Args.Cons[A1, That] =
    Args.Cons(self, that)

  def helpDoc: List[(HelpDoc.Span, HelpDoc.Block)]

  def maxSize: Int

  def minSize: Int

  def validate(args: List[String], opts: ParserOptions): IO[List[HelpDoc.Block], (List[String], A)]
}

object Args {

  final case class Single[+A](pseudoName: String, primType: PrimType[A], description: Vector[String]) extends Args[A] {
    self =>
    def * : Args.Variadic[A] = Args.Variadic(self, None, None)

    def ??(that: String): Single[A] = copy(description = description :+ that)

    def atLeast(min: Int): Args.Variadic[A] = Args.Variadic(self, Some(min), None)

    def atMost(max: Int): Args.Variadic[A] = Args.Variadic(self, None, Some(max))

    def between(min: Int, max: Int): Args.Variadic[A] = Args.Variadic(self, Some(min), Some(max))

    def helpDoc: List[(HelpDoc.Span, HelpDoc.Block)] = {
      import HelpDoc.dsl._

      List(
        spans(weak(pseudoName), space, HelpDoc.dsl.text("(" + primType.render + ")")) ->
          blocks(description.map(p(_)))
      )
    }

    def maxSize: Int = 1

    def minSize: Int = 1

    def validate(args: List[String], opts: ParserOptions): IO[List[HelpDoc.Block], (List[String], A)] = {
      import HelpDoc.dsl._

      args match {
        case head :: tail => primType.validate(head).bimap(text => p(text) :: Nil, a => tail -> a)
        case Nil =>
          IO.fail(p(s"Missing argument <${pseudoName}> of type ${primType.render}.") :: Nil)
      }
    }
  }

  case object Empty extends Args[Unit] {
    def helpDoc: List[(HelpDoc.Span, HelpDoc.Block)] = Nil

    def maxSize: Int = 0

    def minSize: Int = 0

    def validate(args: List[String], opts: ParserOptions): IO[List[HelpDoc.Block], (List[String], Unit)] =
      IO.succeed((args, ()))
  }

  final case class Cons[+A, +B](head: Args[A], tail: Args[B]) extends Args[(A, B)] {
    def helpDoc: List[(HelpDoc.Span, HelpDoc.Block)] = head.helpDoc ++ tail.helpDoc

    def maxSize: Int = head.maxSize + tail.maxSize

    def minSize: Int = head.minSize + tail.minSize

    def validate(args: List[String], opts: ParserOptions): IO[List[HelpDoc.Block], (List[String], (A, B))] =
      for {
        tuple     <- head.validate(args, opts)
        (args, a) = tuple
        tuple     <- tail.validate(args, opts)
        (args, b) = tuple
      } yield (args, (a, b))
  }

  final case class Variadic[+A](value: Single[A], min: Option[Int], max: Option[Int]) extends Args[List[A]] {

    import HelpDoc.dsl._

    // TODO
    def helpDoc: List[(HelpDoc.Span, HelpDoc.Block)] = value.helpDoc.map{
      case (span, block) =>
        val newSpan = span + dsl.text(if (max.isDefined) s" ${minSize} - ${maxSize}" else s"${minSize}")
        val newBlock = blocks(block, p(if (max.isDefined)
          s"This argument must be repeated at least ${minSize} times and up to ${maxSize} times."
        else s"This argument must be repeated at least ${minSize} times."))
        (newSpan,newBlock)
    }


    def maxSize: Int = max.getOrElse(Int.MaxValue / 2) * value.maxSize

    def minSize: Int = min.getOrElse(0) * value.minSize

    def validate(args: List[String], opts: ParserOptions): IO[List[HelpDoc.Block], (List[String], List[A])] = {
      val min1 = min.getOrElse(0)
      val max1 = max.getOrElse(Int.MaxValue)

      def loop(args: List[String], acc: List[A]): IO[List[HelpDoc.Block], (List[String], List[A])] =
        if (acc.length >= max1) IO.succeed(args -> acc)
        else
          value
            .validate(args, opts)
            .foldM(
              failure => if (acc.length >= min1) IO.succeed(args -> acc) else IO.fail(failure),
              { case (args, a) => loop(args, a :: acc) }
            )

      loop(args, Nil).map { case (args, list) => (args, list.reverse) }
    }
  }

  def text(name: String): Single[String] = Single(name, PrimType.Text, Vector.empty)

  def file(name: String, exists: Boolean): Single[JPath] =
    Single(name, PrimType.Path(PrimType.PathType.File, exists), Vector.empty)

  def directory(name: String, exists: Boolean): Single[JPath] =
    Single(name, PrimType.Path(PrimType.PathType.Directory, exists), Vector.empty)

  def int(name: String): Single[BigInt] = Single(name, PrimType.Integer, Vector.empty)

  val empty: Args[Unit] = Empty
}
