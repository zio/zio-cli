package figlet

import zio.Chunk

// FigLettering Font (.flf)
// see: http://www.jave.de/figlet/figfont.html or https://github.com/cmatsuoka/figlet/blob/master/figfont.txt
final case class FigFont(
  header: FigHeader,
  rightToLeft: Boolean,
  layout: Layouts,
  chars: Map[Char, FigChar]
)

object FigFont extends FigFontPlatformSpecific {
  def fromLines(lines: Iterable[String]): Either[String, FigFont] = FigFontParser.parse(Chunk.fromIterable(lines))
}

final case class FigChar(lines: Chunk[FigCharLine], width: Int, height: Int) {
  override def toString: String =
    lines
      .map(l => if (l.leftSpaces == width) " " * width else " " * l.leftSpaces + l.content + " " * l.rightSpaces)
      .mkString("\n")
}

final case class FigCharLine(leftSpaces: Int, rightSpaces: Int, content: String)

object FigCharLine {
  def fromFullLine(width: Int, line: String): FigCharLine = {
    val cut         = line.take(width)
    val leftSpaces  = cut.takeWhile(_ == ' ').count(_ => true)
    val rightSpaces = cut.reverse.takeWhile(_ == ' ').count(_ => true)
    FigCharLine(leftSpaces, rightSpaces, cut.drop(leftSpaces).dropRight(rightSpaces))
  }
}

final case class FigHeader(
  signature: String,
  hardBlank: Char,
  charHeight: Int,
  baseline: Int,
  maxLength: Int,
  commentLines: Int,
  oldLayoutMask: Int,
  rightToLeft: Option[Int],
  fullLayoutMask: Option[Int]
)

sealed abstract class Layout[+SR] extends Product with Serializable
object Layout {
  case object FullWidth                         extends Layout[Nothing]
  case object Fitting                           extends Layout[Nothing]
  final case class Smushing[SR](rules: Seq[SR]) extends Layout[SR]
}

sealed trait SmushingRule           extends Product with Serializable
sealed trait HorizontalSmushingRule extends SmushingRule
sealed trait VerticalSmushingRule   extends SmushingRule
sealed trait BothSmushingRule       extends HorizontalSmushingRule with VerticalSmushingRule
object SmushingRule {
  case object Universal                 extends BothSmushingRule
  case object EqualCharacter            extends BothSmushingRule
  case object Underscore                extends BothSmushingRule
  case object Hierarchy                 extends BothSmushingRule
  case object OppositePair              extends HorizontalSmushingRule
  case object BigX                      extends HorizontalSmushingRule
  case object HardBlank                 extends HorizontalSmushingRule
  case object HorizontalLine            extends VerticalSmushingRule
  case object VerticalLineSuperSmushing extends VerticalSmushingRule
}

final case class Layouts(horizontal: Layout[HorizontalSmushingRule], vertical: Layout[VerticalSmushingRule])

object Layouts {
  import SmushingRule._
  import Layout._

  def fromHeaderMasks(oldLayoutMask: Int, fullLayoutMask: Option[Int]): Layouts = {
    val mask = fullLayoutMask.getOrElse(oldLayoutMask match {
      case 0          => 64      // kerning
      case m if m > 0 => m + 128 // smushing
      case _          => 0       // full width
    })
    Layouts(horizontalLayout(mask), verticalLayout(mask))
  }

  private def horizontalLayout(mask: Int): Layout[HorizontalSmushingRule] = mask match {
    case m if (m & 128) == 128 => Smushing(matchedRulesOrUniversal(horizontalRuleMasks, mask))
    case m if (m & 64) == 64   => Fitting
    case _                     => FullWidth
  }

  private def verticalLayout(mask: Int): Layout[VerticalSmushingRule] = mask match {
    case m if (m & 16384) == 16384 => Smushing(matchedRulesOrUniversal(verticalRuleMasks, mask))
    case m if (m & 8192) == 8192   => Fitting
    case _                         => FullWidth
  }

  private def matchedRulesOrUniversal[SR >: Universal.type](rules: Seq[(Int, SR)], mask: Int) =
    rules.collect { case (i, r) if (mask & i) == i => r } match {
      case Seq() => Seq(Universal)
      case rules => rules
    }

  private final val horizontalRuleMasks = Seq(
    (1, EqualCharacter),
    (2, Underscore),
    (4, Hierarchy),
    (8, OppositePair),
    (16, BigX),
    (32, HardBlank)
  )

  private final val verticalRuleMasks = Seq(
    (256, EqualCharacter),
    (512, Underscore),
    (1024, Hierarchy),
    (2048, HorizontalLine),
    (4096, VerticalLineSuperSmushing)
  )
}
