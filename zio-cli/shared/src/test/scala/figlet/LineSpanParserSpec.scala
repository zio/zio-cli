package figlet

import zio.test._
import zio.test.Assertion._
import FigFontParser._
import ParseResult._

object LineSpanParserSpec extends DefaultRunnableSpec {
  def spec = suite("FigFontParserSpec")(
    suite("line parsing")(
      test("chars") {
        assert(chars(2)(LineSpan("1234")))(equalTo(Ok(LineSpan("1234", 2), "12"))) &&
        assert(chars(2)(LineSpan("1234", 2)))(equalTo(Ok(LineSpan("1234", 4), "34"))) &&
        assert(chars(3).parse(LineSpan("1234", 2)))(isLeft(equalTo("3:EOL before 3th char")))
      },
      test("char") {
        assert(char('b')(LineSpan("abc", 1)))(equalTo(Ok(LineSpan("abc", 2), ()))) &&
        assert(char('a').parse(LineSpan("ab", 1)))(isLeft(equalTo("2:'a' expected, found 'b'")))
      },
      test("token") {
        assert(token(LineSpan("1a2b", 1)))(equalTo(Ok(LineSpan("1a2b", 4), "a2b")))

      },
      test("int") {
        assert(int(LineSpan("a123", 1)))(equalTo(Ok(LineSpan("a123", 4), 123)))
      }
    )
  )
}
