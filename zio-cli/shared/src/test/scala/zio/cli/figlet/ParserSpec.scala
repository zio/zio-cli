package zio.cli.figlet

import zio.Chunk
import zio.test._
import zio.test.Assertion._
import zio.test.ZIOSpecDefault

object ParserSpec extends ZIOSpecDefault {
  import Parser._
  import ParseResult._

  def spec = suite("string parsing")(
    test("word") {
      assert(word("hello"))(equalTo(Ok("", "hello"))) &&
      assert(word("hello world"))(equalTo(Ok(" world", "hello"))) &&
      assert(word.parse("hello"))(isRight(equalTo("hello"))) &&
      assert(word.parse("hello world"))(isRight(equalTo("hello")))
    },
    test("ws") {
      assert(ws.parse(" "))(isRight(equalTo(" "))) &&
      assert(ws.parse("  world"))(isRight(equalTo("  ")))
    },
    test("word <~ ws") {
      val p = word <~ ws
      assert(p.parse("hello "))(isRight(equalTo("hello"))) &&
      assert(p.parse("hello  world"))(isRight(equalTo("hello")))
    },
    test("hello world") {
      val wsw = (word <~ ws) ~ word <~ eol
      assert(wsw.parse("hello world"))(isRight(equalTo(("hello", "world")))) &&
      assert(wsw.parse("hello world "))(isLeft(equalTo(("EOL expected"))))
    },
    test("hello world foreach") {
      val wsw = for {
        w1 <- word
        _  <- ws
        w2 <- word
        _  <- eol
      } yield (w1, w2)
      assert(wsw.parse("hello  world"))(isRight(equalTo(("hello", "world")))) &&
      assert(wsw.parse("hello world "))(isLeft(equalTo("EOL expected"))) &&
      assert(wsw.parse("hello 2"))(isLeft(equalTo("Word expected: '2'")))
    },
    test("many words") {
      val words = for {
        h <- word
        t <- (ws ~> word).*
      } yield h +: t
      assert(words.parse("a b c"))(isRight(equalTo(Chunk("a", "b", "c"))))
    },
    test("N words") {
      val words3 = for {
        w2 <- (word <~ ws).rep(2)
        w1 <- word
      } yield w2 :+ w1
      assert(words3.parse("a b c d"))(isRight(equalTo(Chunk("a", "b", "c")))) &&
      assert(words3.parse("a b"))(isLeft(equalTo("Whitespace expected: ''")))
    }
  )

  type StringParser[A] = Parser[String, String, A]

  final val ws                      = token("Whitespace")(r => r.takeWhile(_.isWhitespace))
  final val word                    = token("Word")(r => r.takeWhile(_.isLetter))
  final val eol: StringParser[Unit] = Parser(r => if (r.isEmpty) Ok(r, ()) else error("EOL expected"))

  def token(name: String)(f: String => String): StringParser[String] = Parser { r =>
    f(r) match {
      case "" => error(s"$name expected: '$r'")
      case s  => Ok(r.drop(s.length), s)
    }
  }
}
