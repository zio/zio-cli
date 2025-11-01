package zio.cli.figlet

import zio.Chunk

// FigLettering Font (.flf)
// see: http://www.jave.de/figlet/figfont.html or https://github.com/cmatsuoka/figlet/blob/master/figfont.txt
final case class FigFont(
  header: FigHeader,
  rightToLeft: Boolean,
  layout: Layouts,
  chars: Map[Char, FigChar]
) { self =>
  final def renderLines(text: String): Chunk[String] = FigFontRenderer.render(self, text)

  final def render(text: String): String = renderLines(text).mkString("\n")
}

object FigFont extends FigFontPlatformSpecific {
  def fromLines(lines: Iterable[String]): Either[String, FigFont] = FigFontParser.parse(Chunk.fromIterable(lines))

  lazy val Default = zio.cli.figlet.FigFontFiles.Default
}

final case class FigChar(lines: Chunk[FigCharLine], width: Int, height: Int) {
  import FigCharLine._

  override def toString: String = fullLines().mkString("\n")

  def fullLines(hardblank: Char = ' ', trim: Boolean = false): Chunk[String] = {
    val lt = if (trim) lines.map(_.spaces(false)).min else 0
    val rt = if (trim) lines.map(_.spaces(true)).min else 0
    lines.map {
      case Empty(w)       => " " * (w - lt - rt)
      case Chars(l, s, r) => " " * (l - lt) + s.replace(hardblank, ' ') + " " * (r - rt)
    }
  }
}

sealed trait FigCharLine {
  import FigCharLine._

  final def spaces(rtl: Boolean): Int = this match {
    case Empty(w)       => w
    case Chars(l, _, r) => if (rtl) r else l
  }
}

object FigCharLine {
  final case class Empty(width: Int)                           extends FigCharLine
  final case class Chars(left: Int, chars: String, right: Int) extends FigCharLine

  def fromFullLine(width: Int, line: String): FigCharLine = {
    val s = line.take(width)
    s.indexWhere(_ != ' ') match {
      case -1 => Empty(s.length)
      case l  =>
        val r = s.length - 1 - s.lastIndexWhere(_ != ' ')
        Chars(l, s.drop(l).dropRight(r), r)
    }
  }
}

final case class FigHeader(
  signature: String,
  hardBlank: Char,
  charHeight: Int,
  baseline: Int,
  commentLines: Int,
  oldLayoutMask: Int,
  rightToLeft: Option[Int],
  fullLayoutMask: Option[Int]
)

sealed trait LayoutDirection
object LayoutDirection {
  sealed trait Horizontal extends LayoutDirection
  sealed trait Vertical   extends LayoutDirection
  sealed trait Both       extends Horizontal with Vertical
}

final case class Layouts(horizontal: Layout[LayoutDirection.Horizontal], vertical: Layout[LayoutDirection.Vertical])

sealed abstract class Layout[+A <: LayoutDirection] extends Product with Serializable
object Layout {
  case object FullWidth                                                    extends Layout[Nothing]
  case object Fitting                                                      extends Layout[Nothing]
  final case class Smushing[+A <: LayoutDirection](rules: SmushingRule[A]) extends Layout[A]
}

final case class SmushingRule[+A <: LayoutDirection](private val mask: Int) extends AnyVal {
  def unapply(r: SmushingRule[_]): Boolean = r.mask == mask || (r.mask & mask) != 0
  override def toString: String            = SmushingRule.names.collect { case (n, r) if r.unapply(this) => n }.mkString(" | ")
}

object SmushingRule {
  import LayoutDirection._

  final val universal                 = SmushingRule[Both](0)
  final val equalCharacter            = SmushingRule[Both](1 + 256)
  final val underscore                = SmushingRule[Both](2 + 512)
  final val hierarchy                 = SmushingRule[Both](4 + 1024)
  final val oppositePair              = SmushingRule[Horizontal](8)
  final val bigX                      = SmushingRule[Horizontal](0)
  final val hardblank                 = SmushingRule[Horizontal](32)
  final val horizontalLine            = SmushingRule[Vertical](2048)
  final val verticalLineSupersmushing = SmushingRule[Vertical](4096)

  abstract class SmushingRuleOps[A <: LayoutDirection](private val r: SmushingRule[A]) {
    final def |[B <: A](r2: SmushingRule[B]): SmushingRule[A] = SmushingRule[A](r.mask | r2.mask)
  }

  implicit final class SmushingRuleOpsH(r: SmushingRule[Horizontal]) extends SmushingRuleOps[Horizontal](r)
  implicit final class SmushingRuleOpsV(r: SmushingRule[Vertical])   extends SmushingRuleOps[Vertical](r)
  implicit final class SmushingRuleOpsB(private val r: SmushingRule[Both]) {
    def |[A <: LayoutDirection](r2: SmushingRule[A]): SmushingRule[A] = SmushingRule[A](r.mask | r2.mask)
  }

  def fromMaskH(mask: Int): SmushingRule[Horizontal] =
    Seq(equalCharacter, underscore, hierarchy, oppositePair, bigX, hardblank)
      .filter(m => (m.mask & mask & (1 + 2 + 4 + 8 + 16 + 32)) != 0)
      .fold(universal)(_ | _)

  def fromMaskV(mask: Int): SmushingRule[Vertical] =
    Seq(equalCharacter, underscore, hierarchy, horizontalLine, verticalLineSupersmushing)
      .filter(m => (m.mask & mask & (256 + 512 + 1024 + 2048 + 4096)) != 0)
      .fold(universal)(_ | _)

  private val names = Seq(
    ("Universal", universal),
    ("Equal Character", equalCharacter),
    ("Underscore", underscore),
    ("Hierarchy", hierarchy),
    ("Opposite Pair", oppositePair),
    ("Big X", bigX),
    ("Hardblank", hardblank),
    ("Horizontal Line", horizontalLine),
    ("Vertical Line Supersmushing", verticalLineSupersmushing)
  )
}

object Layouts {
  import Layout._
  import SmushingRule._

  def fromHeaderMasks(oldLayoutMask: Int, fullLayoutMask: Option[Int]): Layouts = {
    val mask = fullLayoutMask.getOrElse(oldLayoutMask match {
      case 0          => 64      // kerning
      case m if m > 0 => m + 128 // smushing
      case _          => 0       // full width
    })
    Layouts(
      mask match {
        case m if (m & 128) == 128 => Smushing(fromMaskH(m))
        case m if (m & 64) == 64   => Fitting
        case _                     => FullWidth
      },
      mask match {
        case m if (m & 16384) == 16384 => Smushing(fromMaskV(m))
        case m if (m & 8192) == 8192   => Fitting
        case _                         => FullWidth
      }
    )
  }
}
