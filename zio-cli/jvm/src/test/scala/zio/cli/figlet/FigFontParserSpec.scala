package zio.cli.figlet

import Layout._
import SmushingRule._
import zio.test._
import zio.test.Assertion._

object FigFontParserSpec extends DefaultRunnableSpec {
  def spec = suite("FigFontParserSpec")(
    testM("parse standard.flf") {
      for {
        font_        <- FigFont.fromResource("standard.flf", getClass.getClassLoader)
        font: FigFont = font_ // TODO IJ cross-platform projects issue
      } yield {
        assert(font.header)(equalTo(FigHeader("flf2a", '$', 6, 5, 11, 15, Some(0), Some(24463)))) &&
        assert(font.rightToLeft)(isFalse) &&
        assert(font.layout)(
          equalTo(
            Layouts(
              Smushing(equalCharacter | underscore | hierarchy | oppositePair),
              Smushing(equalCharacter | underscore | hierarchy | horizontalLine | verticalLineSupersmushing)
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
    }
  )
}
