package zio.cli

import java.nio.file.{ Path => JPath }

sealed trait Args[+A] { self =>

  def ::[That, A1 >: A](that: Args[That]): Args.Cons[That, A1] =
    Args.Cons(that, self)

  def helpDoc: HelpDoc.Span = ???
}

object Args {

  final case class Single[+A](pseudoName: String, primType: PrimType[A], description: Vector[String]) extends Args[A] {
    self =>
    def * : Args.Variadic[A] = Args.Variadic(self, None, None)

    def ??(that: String): Single[A] = copy(description = description :+ that)

    def atLeast(min: Int): Args.Variadic[A] = Args.Variadic(self, Some(min), None)

    def atMost(max: Int): Args.Variadic[A] = Args.Variadic(self, None, Some(max))

    def between(min: Int, max: Int): Args.Variadic[A] = Args.Variadic(self, Some(min), Some(max))
  }

  case object Empty extends Args[Unit]

  final case class Cons[+A, +B](head: Args[A], tail: Args[B]) extends Args[(A, B)]

  final case class Variadic[+A](value: Single[A], min: Option[Int], max: Option[Int]) extends Args[List[A]]

  def text(name: String): Single[String] = Single(name, PrimType.Text, Vector.empty)

  def file(name: String, exists: Boolean): Single[JPath] =
    Single(name, PrimType.Path(PrimType.PathType.File, exists), Vector.empty)

  def directory(name: String, exists: Boolean): Single[JPath] =
    Single(name, PrimType.Path(PrimType.PathType.Directory, exists), Vector.empty)

  def int(name: String): Single[BigInt] = Single(name, PrimType.Integer, Vector.empty)

  val empty: Args[Unit] = Empty
}
