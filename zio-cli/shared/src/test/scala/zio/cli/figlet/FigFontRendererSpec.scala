package zio.cli.figlet

import FigFontRenderer.render
import Layout._
import LayoutDirection._
import SmushingRule._
import zio.test.Assertion._
import zio.test._
import zio.Chunk

import scala.annotation.tailrec
import zio.test.ZIOSpecDefault

object FigFontRendererSpec extends ZIOSpecDefault {
  private val header = FigHeader("flf2a", '$', 6, 5, 16, 15, Some(0), Some(24463))
  private val font   = testFigFont(
    """
      |J    T    -    |   |
      |  ** *****       | |
      |   *   *   ***   | |
      |*  *   *        $|$|
      | **    *         | |
      |                 | |"""
  )

  override def spec = suite("FigFontRendererSpec")(
    test("FullWidth HLayout") {
      assertTextBlock(
        render(font.withHLayout(FullWidth), "-?TJ-"),
        """
          |    *****  **     |
          |***   *     *  ***|
          |      *  *  *     |
          |      *   **      |
          |                  |"""
      ) &&
      assertTextBlock(
        render(font.withHLayout(FullWidth).copy(rightToLeft = true), "-?TJ-"),
        """
          |    *****  **     |
          |***   *     *  ***|
          |      *  *  *     |
          |      *   **      |
          |                  |"""
      )
    },
    test("Fitting HLayout") {
      assertTextBlock(
        render(font.withHLayout(Fitting), "-?TJ-"),
        """
          | *******   |
          |****   ****|
          |   **  *   |
          |   * **    |
          |           |"""
      ) &&
      assertTextBlock(
        render(font.withHLayout(Fitting).copy(rightToLeft = true), "-?TJ-"),
        """
          | *******   |
          |****   ****|
          |   **  *   |
          |   * **    |
          |           |"""
      )
    },
    test("Equal Char Smushing HLayout") {
      assertTextBlock(
        render(font.withHLayout(Smushing(equalCharacter)), "-?TJ-"),
        """
          |******  |
          |***  ***|
          |  *  *  |
          |  ***   |
          |        |"""
      ) &&
      assertTextBlock(
        render(font.withHLayout(Smushing(equalCharacter)).copy(rightToLeft = true), "-?TJ-"),
        """
          |******  |
          |***  ***|
          |  *  *  |
          |  ***   |
          |        |"""
      )
    },
    test("FullWidth VLayout") {
      val r = render(font.withVLayout(FullWidth), "T\nJ")
      assert(TextBlock(r))(equalTo(textBlock("""
          |*****|
          |  *  |
          |  *  |
          |  *  |
          |     |
          |  **|
          |   *|
          |*  *|
          | ** |
          |    |""")))

    },
    test("Hardblanks") {
      assertTextBlock(
        render(font.withHLayout(Smushing(equalCharacter)), "||"),
        """
          | |  | |
          | |  | |
          | |  | |
          | |  | |
          | |  | |"""
      )

    },
    test("Fitting VLayout") {
      assertTextBlock(
        render(font.withVLayout(Fitting), "T\nJ"),
        """
          |*****|
          |  *  |
          |  *  |
          |  *  |
          |  ** |
          |   *|
          |*  *|
          | ** |
          |    |"""
      )

    },
    test("Equal Char Smushing VLayout") {
      assertTextBlock(
        render(font.withVLayout(Smushing(equalCharacter)), "T\nJ"),
        """
          |*****|
          |  *  |
          |  *  |
          |  ** |
          |   * |
          |*  *|
          | ** |
          |    |"""
      )
    },
    test("Vertical Line Supersmushing") {
      assertTextBlock(
        render(font.withVLayout(Smushing(equalCharacter | verticalLineSupersmushing)), "|\n|"),
        """
          | | |
          | | |
          | | |
          | | |
          | | |
          | | |"""
      )
    },
    test("Universal Smushing") {
      val font = testFigFont("""
          |12|
          | B|
          |AB|
          |A |""").withHLayout(Smushing(universal))
      assertTextBlock(
        render(font, "12"),
        """
          |B|
          |B|
          |A|"""
      ) &&
      assertTextBlock(
        render(font.copy(rightToLeft = true), "12"),
        """
          |B|
          |A|
          |A|"""
      )
    },
    test("Hierarchy Smushing") {
      val font = testFigFont("""
          |Z     I    |
          | _____ ___ |
          ||__  /|_ _||
          |  / /  | | |
          | / /_  | | |
          |/____||___||""").withHLayout(Smushing(equalCharacter | hierarchy))
      assertTextBlock(
        render(font, "ZI"),
        """
          | ________ |
          ||__  /_ _||
          |  / / | | |
          | / /_ | | |
          |/____|___||"""
      )
    }
  )

  private final case class TextBlock(lines: Chunk[String]) {
    override def toString: String = ("" +: lines.map(l => s"|$l|") :+ "").mkString("\n")
  }

  private def textBlock(s: String) = TextBlock(
    Chunk.fromIterable(
      s.stripMargin
        .split("\n")
        .dropWhile(l => l.forall(_.isWhitespace))
        .map(_.stripSuffix("|"))
        .toSeq
    )
  )

  def assertTextBlock(actual: Chunk[String], expected: String): TestResult =
    assert(TextBlock(actual))(equalTo(textBlock(expected)))

  private def testFigFont(s: String) = textBlock(s).lines.splitAt(1) match {
    case (MyNonEmptyChunk(h), lines) =>
      @tailrec def parse(chars: Map[Char, FigChar], from: Int): Map[Char, FigChar] =
        if (from < h.length) {
          val i = h.indexWhere(_ != ' ', from + 1) match {
            case -1 => h.length
            case i  => i
          }
          val width = i - from
          val fc    = FigChar(lines.map(l => FigCharLine.fromFullLine(width, l.substring(from, i))), width, lines.size)
          parse(chars + (h.charAt(from) -> fc), i)
        } else chars

      FigFont(
        header = header.copy(charHeight = lines.size),
        rightToLeft = false,
        Layouts(FullWidth, FullWidth),
        parse(Map.empty, 0)
      )

    case _ => throw new Error("Unbelievable!")
  }

  implicit final class FigFontOps(private val font: FigFont) {
    def withHLayout(hLayout: Layout[Horizontal]): FigFont = font.copy(layout = font.layout.copy(horizontal = hLayout))
    def withVLayout(vLayout: Layout[Vertical]): FigFont   = font.copy(layout = font.layout.copy(vertical = vLayout))
  }

  // TODO remove once https://github.com/zio/zio/pull/4310 released
  object MyNonEmptyChunk {
    def unapplySeq[A](seq: Seq[A]): Option[Seq[A]] =
      seq match {
        case chunk: Chunk[A] if chunk.nonEmpty => Some(chunk)
        case _                                 => None
      }
  }
}
