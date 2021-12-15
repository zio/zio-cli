package zio

import scala.util.{Failure, Success, Try}

package object cli {

  /**
   * `Try` methods that don't exist in 2.11.
   */
  implicit final class TryBackfill[+A](private val self: Try[A]) extends AnyVal {

    def toEither: Either[Throwable, A] = self match {
      case Success(v) => Right(v)
      case Failure(t) => Left(t)
    }

  }

  // Either extension toOption
  implicit final class EitherBackfill[+E, +A](private val self: Either[E, A]) extends AnyVal {

    def toOption: Option[A] = self match {
      case Right(v) => Some(v)
      case Left(_)  => None
    }

  }

  // String extension toIntOption

  implicit final class StringBackfill(private val self: String) extends AnyVal {

    def toIntOption: Option[Int] = Try(self.toInt).toOption

  }

}
