package zio.cli.figlet

/**
 * The result of running a [[Parser]]: `Ok` with the input left over and the parsed value, or `Err` with an error
 * message.
 */
sealed abstract class ParseResult[+R, +A] extends Product with Serializable {
  import ParseResult._

  /** Applies `f` to the value of an `Ok`; returns an `Err` unchanged. */
  final def map[B](f: A => B): ParseResult[R, B] = this match {
    case Ok(r, a) => Ok(r, f(a))
    case e        => e.asInstanceOf[ParseResult[R, B]]
  }

  /** Passes an `Ok`, with its left-over input and value, to `f` and returns the result; returns an `Err` unchanged. */
  def flatMap[R1, B](f: ParseResult.Ok[R, A] => ParseResult[R1, B]): ParseResult[R1, B] = this match {
    case ok: Ok[R, A] => f(ok)
    case e            => e.asInstanceOf[ParseResult[R1, B]]
  }

  /**
   * Returns `Right` with the value of an `Ok`, or `Left` with the message of an `Err`; the left-over input is dropped.
   */
  def toEither: Either[String, A] = this match {
    case Ok(_, a) => Right(a)
    case Err(e)   => Left(e())
  }
}

/** The two kinds of [[ParseResult]], and `error`, which builds an `Err`. */
object ParseResult {

  /** A successful parse: `r` is the input left over and `a` the parsed value. */
  final case class Ok[+R, +A](r: R, a: A) extends ParseResult[R, A]

  /**
   * A failed parse. `e` builds the error message each time it is called; `toString` shows the message, as in
   * `Err(boom)`.
   */
  final case class Err(e: () => String) extends ParseResult[Nothing, Nothing] {
    override def toString: String = s"Err(${e()})"
  }

  /** Returns an `Err` whose message `e` is computed each time it is read (by `toEither` or `toString`), not before. */
  def error(e: => String): Err = Err(() => e)
}

/**
 * A parser: a function from the input `R` to a [[ParseResult]] with the input left over, of type `R1`, and a value of
 * type `A`. Parsers combine with `map`, `flatMap` (so `for` comprehensions work), `~`, `~>` and `<~`;
 * `FigFont.fromLines` reads FIGfont files with them.
 *
 * {{{
 * val digit: Parser[String, String, Char] = Parser { (s: String) =>
 *   if (s.nonEmpty && s.head.isDigit) ParseResult.Ok(s.tail, s.head)
 *   else ParseResult.error("digit expected: '" + s + "'")
 * }
 * val twoDigits: Parser[String, String, (Char, Char)] = digit ~ digit
 *
 * twoDigits.parse("12x") // Right((1,2))
 * twoDigits.parse("1x")  // Left(digit expected: 'x')
 * }}}
 */
abstract class Parser[-R, +R1, +A] extends (R => ParseResult[R1, A]) {

  /**
   * Runs this parser on `r` and returns its value as `Right`, or its error message as `Left`. Left-over input is
   * ignored.
   */
  final def parse(r: R): Either[String, A] = this(r).toEither

  /** Returns a parser that applies `f` to the value of this parser. */
  final def map[B](f: A => B): Parser[R, R1, B] = (r: R) => this(r).map(f)

  /** Returns a parser that runs this parser, then the parser that `f` builds from its value, on the input left over. */
  final def flatMap[R2, B](f: A => Parser[R1, R2, B]): Parser[R, R2, B] = (r: R) => this(r).flatMap(ok => f(ok.a)(ok.r))

  /** Returns a parser that runs this parser, then `p` on the input left over, and returns both values as a pair. */
  final def ~[R2, B](p: Parser[R1, R2, B]): Parser[R, R2, (A, B)] = flatMap(a => p.map((a, _)))

  /** Returns a parser that runs this parser, then `p`, and keeps only the value of `p`. */
  final def ~>[R2, B](p: Parser[R1, R2, B]): Parser[R, R2, B] = flatMap(_ => p)

  /** Returns a parser that runs this parser, then `p`, and keeps only the value of this parser. */
  final def <~[R2, B](p: Parser[R1, R2, B]): Parser[R, R2, A] = flatMap(a => p.map(_ => a))
}

/** Creates parsers, and adds optional and repeated parsing with [[Parser.ParserOps]]. */
object Parser {
  import ParseResult._
  import scala.annotation.tailrec
  import zio.{Chunk, ChunkBuilder}

  /** Creates a parser from the function `f`. */
  def apply[R, R1, A](f: R => ParseResult[R1, A]): Parser[R, R1, A] = (r: R) => f(r)

  /** Adds `?`, `*`, `rep` and `refine` to a parser whose left-over input has the same type as its input. */
  implicit final class ParserOps[R, A](private val p: Parser[R, R, A]) extends AnyVal {

    /**
     * Returns a parser that makes this one optional: `Some` with its value when it succeeds, or `None` with the input
     * unconsumed when it fails.
     */
    def ? : Parser[R, R, Option[A]] = Parser { r =>
      p(r) match {
        case Ok(r, a) => Ok(r, Some(a))
        case _        => Ok(r, None)
      }
    }

    /**
     * Returns a parser that runs this one again and again on the input left over until it fails, and returns the values
     * in order (possibly none) with the input where it failed. It doesn't stop while this parser succeeds, so with a
     * parser that succeeds without consuming input it doesn't terminate.
     */
    def * : Parser[R, R, Chunk[A]] = {
      @tailrec def acc(r: R, s: Chunk[A]): Ok[R, Chunk[A]] = p(r) match {
        case Ok(r, a) => acc(r, s :+ a)
        case _        => Ok(r, s)
      }

      Parser(r => acc(r, Chunk.empty))
    }

    /**
     * Returns a parser that runs this one exactly `n` times and returns the `n` values in order, or the first error.
     * Calling `rep` with `n` of 0 or less throws a `java.lang.AssertionError`.
     */
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

    /**
     * Returns a parser that runs this one and checks its value with `f`. `Right(b)` gives `b`; `Left(err)` fails with
     * `err` applied to the input this parser started from.
     */
    def refine[B](f: A => Either[R => Err, B]): Parser[R, R, B] = Parser { r =>
      p(r).flatMap(ok => f(ok.a).fold(re => re(r), Ok(ok.r, _)))
    }
  }
}
