package zio.cli.figlet

import zio.Chunk
import scala.math.{max, min}
import Layout._
import SmushingRule._
import FigCharLine._

private[figlet] object FigFontRenderer {
  def render(font: FigFont, text: String): Chunk[String] = {
    val (hb, h, rtl)              = (font.header.hardBlank, font.header.charHeight, font.rightToLeft)
    val Layouts(hLayout, vLayout) = font.layout
    val emptyFigChar              = FigChar(Chunk.fill(h)(Empty(0)), 0, h)
    val missingFigChar            = font.chars.getOrElse('\u0000', emptyFigChar)

    // TODO add renderOptions e.g. maxWidth to implement word-wrapping
    // TODO review whether to trim vertically (or only at the bottom) given we trim horizontally
    def renderLine(line: String): Chunk[Chunk[String]] = Chunk(
      line
        .map(font.chars.getOrElse(_, missingFigChar))
        .fold(emptyFigChar)(hAppend)
        .fullLines(trim = true)
    )

    def hAppend(b1: FigChar, b2: FigChar): FigChar = {
      val f     = measure(hb, hLayout, rtl, h, b1, b2)
      val lines = b1.lines.zipWith(b2.lines) { (l1, l2) =>
        (l1, l2) match {
          case (Empty(r), Empty(l))                 => Empty(r - f + l)
          case (Empty(r), Chars(l, s2, r2))         => Chars(r - f + l, s2, r2)
          case (Chars(l1, s1, r), Empty(l))         => Chars(l1, s1, r - f + l)
          case (Chars(l1, s1, r), Chars(l, s2, r2)) =>
            Chars(
              l1,
              hLayout match {
                case Smushing(rules) if r - f + l < 0 =>
                  s1.dropRight(1) + smush(hb, rtl, rules, s1.last, s2.head) + s2.drop(1)
                case _ => s1 + " " * (r - f + l) + s2
              },
              r2
            )
        }
      }
      FigChar(lines, b1.width + b2.width - f, h)
    }

    def vAppend(b1: Chunk[String], b2: Chunk[String]): Chunk[String] = {
      val f     = measure(hb, vLayout, rtl = false, height = max(b1(0).length, b2(0).length), b1, b2)
      val merge = b1.takeRight(f).zipWith(b2.take(f)) { (l1, l2) =>
        val chars = Array.tabulate(min(l1.length, l2.length))(i =>
          (l1.charAt(i), l2.charAt(i)) match {
            case (' ', c) => c
            case (c, ' ') => c
            case (c1, c2) =>
              vLayout match {
                case Smushing(rules) => smush(hb, rtl = false, rules, c1, c2)
                case _               => ??? // measure allows to overlap only for Smushing
              }
          }
        )
        new String(chars) + l1.drop(chars.length) + l2.drop(chars.length)
      }
      b1.dropRight(f) ++ merge ++ b2.drop(f)
    }

    text
      .split("\n")
      .flatMap(renderLine)
      .reduce(vAppend)
      .map(_.replace(hb, ' '))
  }

  private trait Block[A] {
    def width(a: A): Int
    def spaces(a: A, row: Int, rtl: Boolean): Int
    def charAt(a: A, row: Int, n: Int, rtl: Boolean): Char
  }

  private val MISSING = '\uDBFF'

  private implicit object FigCharBlock extends Block[FigChar] {
    override def width(a: FigChar): Int = a.width

    override def spaces(a: FigChar, row: Int, rtl: Boolean): Int = a.lines(row).spaces(rtl)

    override def charAt(a: FigChar, row: Int, n: Int, rtl: Boolean): Char = a.lines(row) match {
      case Empty(w)       => if (n < w) ' ' else MISSING
      case Chars(l, s, r) =>
        val i = if (rtl) s.length - 1 - (n - r) else n - l
        if (0 <= i && i < s.length) s.charAt(i) else MISSING
    }
  }

  private implicit object StringChunkBlock extends Block[Chunk[String]] {
    override def width(a: Chunk[String]): Int = a.size

    override def spaces(a: Chunk[String], row: Int, rtl: Boolean): Int =
      if (rtl) a.lastIndexWhere(l => row < l.length && l.charAt(row) != ' ') match {
        case -1 => a.size
        case i  => a.size - 1 - i
      }
      else
        a.indexWhere(l => l.length > row && l.charAt(row) != ' ') match {
          case -1 => a.size
          case i  => i
        }

    override def charAt(a: Chunk[String], row: Int, n: Int, rtl: Boolean): Char =
      a(if (rtl) a.size - 1 - n else n).charAt(row)
  }

  private def measure[A](hb: Char, layout: Layout[_], rtl: Boolean, height: Int, b1: A, b2: A)(implicit ev: Block[A]) =
    layout match {
      case FullWidth       => 0
      case Fitting         => (0 until height).map(r => ev.spaces(b1, r, rtl = true) + ev.spaces(b2, r, rtl = false)).min
      case Smushing(rules) =>
        (0 until height).foldLeft(ev.width(b1) + ev.width(b2)) { (fitting, r) =>
          val s1 = ev.spaces(b1, r, rtl = true)
          val s2 = ev.spaces(b2, r, rtl = false)
          s1 + s2 match {
            case f if f >= fitting => fitting
            case f                 =>
              (ev.charAt(b1, r, s1, rtl = true), ev.charAt(b2, r, s2, rtl = false)) match {
                case (MISSING, _) | (_, MISSING) => f
                case (c1, c2)                    =>
                  smush(hb, rtl, rules, c1, c2) match {
                    case MISSING => f
                    // vertical line supersmushing can smush all vertically adjacent |
                    case '|' if verticalLineSupersmushing.unapply(rules) && c1 == '|' && c2 == '|' =>
                      val rng = 1 until fitting - f
                      f + 1 +
                        rng.takeWhile(i => ev.charAt(b1, r, s1 + i, rtl = true) == '|').length +
                        rng.takeWhile(i => ev.charAt(b2, r, s2 + i, rtl = false) == '|').length
                    case _ => f + 1
                  }
              }
          }
        }
    }

  private def smush(hb: Char, rtl: Boolean, rules: SmushingRule[_], a: Char, b: Char): Char = rules match {
    case universal()                                                                    => if (a != hb && b != hb) if (rtl) a else b else MISSING
    case equalCharacter() if a == b && a != hb                                          => b
    case hardblank() if a == hb && b == hb                                              => hb
    case underscore() if a == '_' && "|/\\[]{}()<>".contains(b)                         => b
    case bigX() if eq(pos(a, "/\\>"), pos(b, "\\/<"))                                   => "|YX".charAt(pos(a, "/\\>"))
    case oppositePair() if eq(pos(a, "[]{}()"), pos(b, "][}{)("))                       => '|'
    case hierarchy() if notEq(pos(a, "||/\\[]{}()<>") / 2, pos(b, "||/\\[]{}()<>") / 2) =>
      if (pos(a, "||/\\[]{}()<>") > pos(b, "||/\\[]{}()<>")) a else b // TODO review if rtl effects this case
    case horizontalLine() if a == '-' && b == '_' || a == '_' && b == '-' => '='
    case verticalLineSupersmushing() if a == '|' && b == '|'              => '|'
    case _                                                                => MISSING
  }

  private def pos(c: Char, s: String) = s.indexOf(Char.char2int(c))
  private def eq(a: Int, b: Int)      = a >= 0 && a == b
  private def notEq(a: Int, b: Int)   = a >= 0 && b >= 0 && a != b
}
