package zio

import scala.util.{Failure, Success, Try}

package object cli {

  /**
   * `Try` methods that don't exist in 2.11.
   */
  implicit final class TryBackfill[+A](private val self: Try[A]) extends AnyVal {

    /** Returns `Right` with the value of a `Success`, or `Left` with the exception of a `Failure`. */
    def toEither: Either[Throwable, A] = self match {
      case Success(v) => Right(v)
      case Failure(t) => Left(t)
    }

  }

  /**
   * Adds `toOption` to `Either`, which Scala 2.11 does not have.
   */
  implicit final class EitherBackfill[+E, +A](private val self: Either[E, A]) extends AnyVal {

    /** Returns the value of a `Right` in a `Some`, or `None` for a `Left`. */
    def toOption: Option[A] = self match {
      case Right(v) => Some(v)
      case Left(_)  => None
    }

  }

  /**
   * Adds `toIntOption` to `String`, which Scala 2.11 does not have.
   */
  implicit final class StringBackfill(private val self: String) extends AnyVal {

    /**
     * Returns the string parsed as an `Int`, or `None` when `toInt` throws (for example for `"12x"`, or a number out
     * of the `Int` range).
     */
    def toIntOption: Option[Int] = Try(self.toInt).toOption

  }

}
