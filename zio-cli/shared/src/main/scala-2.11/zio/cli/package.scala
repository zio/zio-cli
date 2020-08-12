package zio

import scala.util.{Failure, Success, Try}

package object cli {

  /**
   * `Try` methods that don't exist in 2.11.
   */
  implicit final class TryBackfill[+A](private val t: Try[A]) extends AnyVal {

    def toEither: Either[Throwable, A] = t match {
      case Success(v) => Right(v)
      case Failure(t) => Left(t)
    }

  }

}
