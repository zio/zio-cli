package zio.cli

import java.nio.file.{Path => JPath}

sealed trait Args[+A] { self =>

  def :: [That, A1 >: A](that: Args[That]): Args.Cons[That, A1] =
    Args.Cons(that, self)
}

object Args {

  sealed trait Primitive[+A] extends Args[A] { self =>
    def * : Args.Variadic[A] = Args.Variadic(self)
  }

  case object Empty extends Args[Unit]

  object Primitive {
    case object Text extends Primitive[String]
    case object Path extends Primitive[JPath]
    case object Int  extends Primitive[scala.Int]
  }

  final case class Cons[+A, +B](head: Args[A], tail: Args[B]) extends Args[(A, B)]

  final case class Variadic[+A](value: Primitive[A]) extends Args[List[A]]

  val text: Primitive[String] = Primitive.Text
  val path: Primitive[JPath] = Primitive.Path
  val int: Primitive[scala.Int] = Primitive.Int

  val empty: Args[Unit] = Empty
}
