package zio.cli

import java.nio.file.{ Path => JPath }

sealed trait Args[+A] { self =>

  def ::[That, A1 >: A](that: Args[That]): Args.Cons[That, A1] =
    Args.Cons(that, self)

  def helpDoc: HelpDoc.Span = ???
}

object Args {

  final case class Single[+A](pseudoName: String, argType: Type[A]) extends Args[A] { self =>
    def * : Args.Variadic[A] = Args.Variadic(self, None, None)

    def atLeast(min: Int): Args.Variadic[A] = Args.Variadic(self, Some(min), None)

    def atMost(max: Int): Args.Variadic[A] = Args.Variadic(self, None, Some(max))

    def between(min: Int, max: Int): Args.Variadic[A] = Args.Variadic(self, Some(min), Some(max))
  }

  case object Empty extends Args[Unit]

  sealed trait Type[+A]
  object Type {
    case object Text extends Type[String]
    case object Path extends Type[JPath]
    case object Int  extends Type[scala.Int]
  }

  final case class Cons[+A, +B](head: Args[A], tail: Args[B]) extends Args[(A, B)]

  final case class Variadic[+A](value: Single[A], min: Option[Int], max: Option[Int]) extends Args[List[A]]

  def text(name: String): Single[String]   = Single(name, Type.Text)
  def path(name: String): Single[JPath]    = Single(name, Type.Path)
  def int(name: String): Single[scala.Int] = Single(name, Type.Int)

  val empty: Args[Unit] = Empty
}
