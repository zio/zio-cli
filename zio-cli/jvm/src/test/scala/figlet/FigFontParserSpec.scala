package figlet

import zio.test.Assertion._
import zio.test._

object FigFontParserSpec extends DefaultRunnableSpec {
  import FigFontParser._
  import ParseResult._
  import Layout._
  import SmushingRule._

  def spec = suite("FigFontParserSpec")(
    testM("parse standard.flf") {
      for {
        font_         <- FigFont.fromResource("standard.flf", getClass.getClassLoader)
        font: FigFont = font_ // TODO IJ cross-platform projects issue
      } yield {
        assert(font.header)(equalTo(FigHeader("flf2a", '$', 6, 5, 16, 11, 15, Some(0), Some(24463)))) &&
        assert(font.rightToLeft)(isFalse) &&
        assert(font.layout)(
          equalTo(
            Layouts(
              Smushing(Seq(EqualCharacter, Underscore, Hierarchy, OppositePair)),
              Smushing(Seq(EqualCharacter, Underscore, Hierarchy, HorizontalLine, VerticalLineSuperSmushing))
            )
          )
        ) &&
        assert(font.chars('Z').toString)(
          equalTo(
            """|  _____|
               | |__  /|
               |   / / |
               |  / /_ |
               | /____||
               |       """.stripMargin
              .replace("\r\n", "\n")
              .replace("|\n", "\n")
          )
        ) &&
        assert(font.chars('\u017E').toString)(
          equalTo(
            """|  \\//|
               |  _\/_|
               | |_  /|
               |  / / |
               | /___||
               |      """.stripMargin
              .replace("\r\n", "\n")
              .replace("|\n", "\n")
          )
        )
      }
//    } @@ TestAspect.ignore,
    },
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
