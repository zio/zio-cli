package zio.cli.figlet

sealed abstract class ParseResult[+R, +A] extends Product with Serializable {
  import ParseResult._

  final def map[B](f: A => B): ParseResult[R, B] = this match {
    case Ok(r, a) => Ok(r, f(a))
    case e        => e.asInstanceOf[ParseResult[R, B]]
  }
  def flatMap[R1, B](f: ParseResult.Ok[R, A] => ParseResult[R1, B]): ParseResult[R1, B] = this match {
    case ok: Ok[R, A] => f(ok)
    case e            => e.asInstanceOf[ParseResult[R1, B]]
  }

  def toEither: Either[String, A] = this match {
    case Ok(_, a) => Right(a)
    case Err(e)   => Left(e())
  }
}

object ParseResult {
  final case class Ok[+R, +A](r: R, a: A) extends ParseResult[R, A]

  final case class Err(e: () => String) extends ParseResult[Nothing, Nothing] {
    override def toString: String = s"Err(${e()})"
  }

  def error(e: => String): Err = Err(() => e)
}

abstract class Parser[-R, +R1, +A] extends (R => ParseResult[R1, A]) {
  final def parse(r: R): Either[String, A]                              = this(r).toEither
  final def map[B](f: A => B): Parser[R, R1, B]                         = (r: R) => this(r).map(f)
  final def flatMap[R2, B](f: A => Parser[R1, R2, B]): Parser[R, R2, B] = (r: R) => this(r).flatMap(ok => f(ok.a)(ok.r))
  final def ~[R2, B](p: Parser[R1, R2, B]): Parser[R, R2, (A, B)]       = flatMap(a => p.map((a, _)))
  final def ~>[R2, B](p: Parser[R1, R2, B]): Parser[R, R2, B]           = flatMap(_ => p)
  final def <~[R2, B](p: Parser[R1, R2, B]): Parser[R, R2, A]           = flatMap(a => p.map(_ => a))
}

object Parser {
  import ParseResult._
  import scala.annotation.tailrec
  import zio.{ Chunk, ChunkBuilder }

  def apply[R, R1, A](f: R => ParseResult[R1, A]): Parser[R, R1, A] = (r: R) => f(r)

  implicit final class ParserOps[R, A](private val p: Parser[R, R, A]) extends AnyVal {
    def ? : Parser[R, R, Option[A]] = Parser { r =>
      p(r) match {
        case Ok(r, a) => Ok(r, Some(a))
        case _        => Ok(r, None)
      }
    }

    def * : Parser[R, R, Chunk[A]] = {
      @tailrec def acc(r: R, s: Chunk[A]): Ok[R, Chunk[A]] = p(r) match {
        case Ok(r, a) => acc(r, s :+ a)
        case _        => Ok(r, s)
      }

      Parser(r => acc(r, Chunk.empty))
    }

    def rep(n: Int): Parser[R, R, Chunk[A]] = {
      assert(n > 0)

      @tailrec def nAccTo(r: R, cb: ChunkBuilder[A], n: Int): ParseResult[R, Any] = p(r) match {
        case Ok(r, a) =>
          cb += a
          if (n == 1) Ok(r, ()) else nAccTo(r, cb, n - 1)
        case e => e
      }

      Parser { r =>
        val cb = ChunkBuilder.make[A](n)
        nAccTo(r, cb, n).map(_ => cb.result())
      }
    }

    def refine[B](f: A => Either[R => Err, B]): Parser[R, R, B] = Parser { r =>
      p(r).flatMap(ok => f(ok.a).fold(re => re(r), Ok(ok.r, _)))
    }
  }
}
