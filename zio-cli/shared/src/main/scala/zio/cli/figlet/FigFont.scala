package zio.cli.figlet

import zio.Chunk

/**
 * A FIGlet lettering font (FIGfont, a `.flf` file): it draws text as large letters made of ASCII characters, like the
 * banner at the top of a [[zio.cli.CliApp]] help screen.
 *
 * Use `FigFont.Default` or parse a font with `FigFont.fromLines` (on the JVM also `FigFont.fromFile`,
 * `FigFont.fromResource`, `FigFont.fromURL` or `FigFont.fromSource`), then call `render` or `renderLines`.
 *
 * {{{
 * val banner: String = FigFont.Default.render("zio")
 * //         _
 * //  ____  (_)___
 * // /_  / / / __ \
 * //  / /_/ / /_/ /
 * // /___/_/\____/
 * }}}
 *
 * @param header
 *   the header line of the font file
 * @param rightToLeft
 *   `true` when the header's print direction is 1 (right to left)
 * @param layout
 *   how neighbouring characters and lines are joined, decoded from the header's layout masks
 * @param chars
 *   the drawing of each character the font defines
 * @see
 *   [[https://github.com/cmatsuoka/figlet/blob/master/figfont.txt FIGfont specification]]
 * @see
 *   [[http://www.jave.de/figlet/figfont.html FIGfont specification (HTML)]]
 */
final case class FigFont(
  header: FigHeader,
  rightToLeft: Boolean,
  layout: Layouts,
  chars: Map[Char, FigChar]
) { self =>

  /**
   * Draws `text` in this font and returns the rows of the drawing, from top to bottom. Each line of `text` (split on
   * `"\n"`) is drawn `header.charHeight` rows high, and lines are joined with the vertical layout, so their rows can
   * overlap. Blank columns shared by all rows of a line are trimmed at both ends, and hardblanks become spaces.
   *
   * A character the font lacks is drawn as the font's character with code 0, or left out when the font has none, as
   * `FigFont.Default` does (`"a€b"` renders like `"ab"`). An empty `text` gives `header.charHeight` empty rows, and a
   * trailing newline is ignored. Text made only of newlines throws `java.lang.UnsupportedOperationException`. With
   * `FigFont.Default`, text with an empty line before another line (`"\na"`, `"a\n\nb"`) throws
   * `java.lang.ArrayIndexOutOfBoundsException`.
   *
   * {{{
   * import zio.Chunk
   *
   * val rows: Chunk[String] = FigFont.Default.renderLines("zio")
   * // rows.size == 6
   * // rows(2)   == "/_  / / / __ \\"
   * }}}
   */
  final def renderLines(text: String): Chunk[String] = FigFontRenderer.render(self, text)

  /** Draws `text` in this font like `renderLines` and joins the rows with `"\n"`. */
  final def render(text: String): String = renderLines(text).mkString("\n")
}

/**
 * Creates [[FigFont]] values: `Default`, the bundled font, and `fromLines`, which parses FIGfont text. On the JVM it
 * also reads fonts from files, class-path resources and URLs.
 */
object FigFont extends FigFontPlatformSpecific {

  /**
   * Parses the lines of a FIGfont (`.flf`) file into a font. On failure it returns `Left` with a message that starts
   * with the line number, and for an error inside a line also the column (`1:17:' ' expected, but found EOL`).
   *
   * The header line must hold the 5-character signature (not checked, usually `flf2a`), the hardblank character and at
   * least 5 integers (height, baseline, max length, old layout, comment lines). After the comment lines come the 102
   * required characters (codes 32 to 126, then `ÄÖÜßäöü`), then characters tagged with their code; reading stops at the
   * first tagged character that does not parse, and the lines after it are ignored.
   *
   * {{{
   * val font: Either[String, FigFont] = FigFont.fromLines(List("flf2a# 6 5 16 15"))
   * // Left(1:17:' ' expected, but found EOL)
   * }}}
   */
  def fromLines(lines: Iterable[String]): Either[String, FigFont] = FigFontParser.parse(Chunk.fromIterable(lines))

  /**
   * The font that [[zio.cli.CliApp]] uses for its banner when no other font is given: the FIGlet font Slant, the same
   * value as `FigFontFiles.Default`. It has the characters with codes 32 to 126 and 160 to 255.
   */
  lazy val Default = zio.cli.figlet.FigFontFiles.Default
}

/**
 * The drawing of one character of a [[FigFont]]: `height` rows (`lines`) and a `width` in columns. Its `toString` is
 * the rows joined with newlines.
 */
final case class FigChar(lines: Chunk[FigCharLine], width: Int, height: Int) {
  import FigCharLine._

  override def toString: String = fullLines().mkString("\n")

  /**
   * Returns the rows of this character as strings, with their leading and trailing spaces. Each `hardblank` character
   * in a row becomes a space; the default `' '` changes nothing. With `trim = true`, the leading and trailing blank
   * columns that all rows share are removed.
   */
  def fullLines(hardblank: Char = ' ', trim: Boolean = false): Chunk[String] = {
    val lt = if (trim) lines.map(_.spaces(false)).min else 0
    val rt = if (trim) lines.map(_.spaces(true)).min else 0
    lines.map {
      case Empty(w)       => " " * (w - lt - rt)
      case Chars(l, s, r) => " " * (l - lt) + s.replace(hardblank, ' ') + " " * (r - rt)
    }
  }
}

/**
 * One row of a [[FigChar]]: either `Empty`, a row of spaces only, or `Chars`, visible characters between leading and
 * trailing spaces.
 */
sealed trait FigCharLine {
  import FigCharLine._

  /**
   * Returns the number of spaces at the start of this row, or at its end when `rtl` is `true`. For an `Empty` row it
   * returns the row's whole width.
   */
  final def spaces(rtl: Boolean): Int = this match {
    case Empty(w)       => w
    case Chars(l, _, r) => if (rtl) r else l
  }
}

/** The two kinds of [[FigCharLine]], and `fromFullLine`, which splits a row read from a font file. */
object FigCharLine {

  /** A row with no visible character, `width` spaces wide. */
  final case class Empty(width: Int) extends FigCharLine

  /**
   * A row with `left` leading spaces, then `chars` (from the first to the last non-space character, inner spaces kept),
   * then `right` trailing spaces.
   */
  final case class Chars(left: Int, chars: String, right: Int) extends FigCharLine

  /**
   * Splits the first `width` characters of `line` into a row: `Empty` when they are all spaces, otherwise `Chars` with
   * the counts of leading and trailing spaces. `fromFullLine(5, "  ab   x")` gives `Chars(2, "ab", 1)`, and a `line`
   * shorter than `width` is used as it is (`fromFullLine(10, "ab")` gives `Chars(0, "ab", 0)`).
   */
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

/**
 * The header line of a FIGfont file, such as `flf2a\$ 6 5 16 15 10 0 18319`. The max-length field is read but not kept.
 *
 * @param signature
 *   the first 5 characters of the line, usually `flf2a`
 * @param hardBlank
 *   the character for a blank that fitting and smushing treat as visible; rendering turns it into a space
 * @param charHeight
 *   the number of rows of every character
 * @param baseline
 *   the number of rows from the top of a character to its baseline
 * @param commentLines
 *   the number of comment lines after the header
 * @param oldLayoutMask
 *   the old layout field: -1 for full width, 0 for fitting, a positive value for smushing with those horizontal rules
 * @param rightToLeft
 *   the print direction (0 left to right, 1 right to left), or `None` when the header doesn't have it
 * @param fullLayoutMask
 *   the full layout field, which replaces `oldLayoutMask` when present
 */
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

/**
 * Marks the direction that a [[Layout]] or a [[SmushingRule]] applies to: between neighbouring characters
 * (`Horizontal`) or between lines of text (`Vertical`). It exists only as a type parameter.
 */
sealed trait LayoutDirection

/** The [[LayoutDirection]] types. */
object LayoutDirection {

  /** Joining neighbouring characters of a line. */
  sealed trait Horizontal extends LayoutDirection

  /** Joining lines of text drawn one above the other. */
  sealed trait Vertical extends LayoutDirection

  /** Both directions: a `SmushingRule[Both]` can be used where a horizontal or a vertical rule is expected. */
  sealed trait Both extends Horizontal with Vertical
}

/**
 * The horizontal and the vertical [[Layout]] of a [[FigFont]], usually decoded from its header by
 * `Layouts.fromHeaderMasks`.
 */
final case class Layouts(horizontal: Layout[LayoutDirection.Horizontal], vertical: Layout[LayoutDirection.Vertical])

/**
 * How two neighbouring characters (horizontal) or two lines of text (vertical) are joined: `FullWidth`, `Fitting` or
 * `Smushing`.
 */
sealed abstract class Layout[+A <: LayoutDirection] extends Product with Serializable

/** The three kinds of [[Layout]]. */
object Layout {

  /** Places the two drawings next to each other at their full width (or height), with no overlap. */
  case object FullWidth extends Layout[Nothing]

  /** Moves the two drawings together until they touch, without overlapping visible characters (kerning). */
  case object Fitting extends Layout[Nothing]

  /**
   * Moves the two drawings closer than `Fitting`, usually by one column (or row), so that touching characters merge
   * into one where `rules` allow it; where no rule applies, they are joined as with `Fitting`.
   */
  final case class Smushing[+A <: LayoutDirection](rules: SmushingRule[A]) extends Layout[A]
}

/**
 * A set of FIGfont smushing rules for direction `A`, stored as a bit mask that uses the FIGfont code values (horizontal
 * rules 1 to 32, vertical rules 256 to 4096). Combine rules with `|`; `toString` lists their names, such as
 * `Equal Character | Underscore`. The empty set is `universal`, and its `toString` is `Universal | Big X`.
 */
final case class SmushingRule[+A <: LayoutDirection](private val mask: Int) extends AnyVal {

  /**
   * Returns `true` when `r` has the same mask as this rule set or shares at least one rule with it, so a rule can be
   * used as a pattern (`case SmushingRule.equalCharacter() =>`). `universal.unapply(r)` is `true` only when `r` has no
   * rule.
   */
  def unapply(r: SmushingRule[_]): Boolean = r.mask == mask || (r.mask & mask) != 0
  override def toString: String            = SmushingRule.names.collect { case (n, r) if r.unapply(this) => n }.mkString(" | ")
}

/** The FIGfont smushing rules, the `|` operator that combines them, and their decoding from a header mask. */
object SmushingRule {
  import LayoutDirection._

  /**
   * No specific rule (mask 0): universal smushing. Two touching characters merge into the later one (the earlier one in
   * a right-to-left font), unless one of them is the hardblank.
   */
  final val universal = SmushingRule[Both](0)

  /** Rule 1 (codes 1 and 256): two equal characters, other than the hardblank, merge into one. */
  final val equalCharacter = SmushingRule[Both](1 + 256)

  /**
   * Rule 2 (codes 2 and 512): an underscore followed by (or above) one of `|/\[]{}()<>` merges into that character.
   */
  final val underscore = SmushingRule[Both](2 + 512)

  /**
   * Rule 3 (codes 4 and 1024): two characters from different classes of `|`, `/\`, `[]`, `{}`, `()` and `<>` merge into
   * the one whose class comes later in that list.
   */
  final val hierarchy = SmushingRule[Both](4 + 1024)

  /** Horizontal rule 4 (code 8): `[]`, `][`, `{}`, `}{`, `()` or `)(` merges into `|`. */
  final val oppositePair = SmushingRule[Horizontal](8)

  /**
   * Horizontal rule 5, Big X. Its mask is 0, the same as `universal`: `bigX == universal`, `fromMaskH` doesn't read the
   * FIGfont code value 16, and rendering never applies the Big X merges.
   */
  final val bigX = SmushingRule[Horizontal](0)

  /** Horizontal rule 6 (code 32): two hardblanks merge into one hardblank. */
  final val hardblank = SmushingRule[Horizontal](32)

  /** Vertical rule 4 (code 2048): `-` above `_`, or `_` above `-`, merges into `=`. */
  final val horizontalLine = SmushingRule[Vertical](2048)

  /** Vertical rule 5 (code 4096): `|` above `|` merges into one `|`. */
  final val verticalLineSupersmushing = SmushingRule[Vertical](4096)

  /** Adds `|` to a rule set of one direction. */
  abstract class SmushingRuleOps[A <: LayoutDirection](private val r: SmushingRule[A]) {

    /** Returns the rules of this set and of `r2` together, with the direction of this set. */
    final def |[B <: A](r2: SmushingRule[B]): SmushingRule[A] = SmushingRule[A](r.mask | r2.mask)
  }

  /** Adds `|` to horizontal rule sets: `oppositePair | hardblank` is a `SmushingRule[Horizontal]`. */
  implicit final class SmushingRuleOpsH(r: SmushingRule[Horizontal]) extends SmushingRuleOps[Horizontal](r)

  /** Adds `|` to vertical rule sets: `horizontalLine | verticalLineSupersmushing` is a `SmushingRule[Vertical]`. */
  implicit final class SmushingRuleOpsV(r: SmushingRule[Vertical]) extends SmushingRuleOps[Vertical](r)

  /** Adds `|` to rule sets that apply in both directions. */
  implicit final class SmushingRuleOpsB(private val r: SmushingRule[Both]) {

    /**
     * Returns the rules of this set and of `r2` together, with the direction of `r2`: `equalCharacter | oppositePair`
     * is a `SmushingRule[Horizontal]`.
     */
    def |[A <: LayoutDirection](r2: SmushingRule[A]): SmushingRule[A] = SmushingRule[A](r.mask | r2.mask)
  }

  /**
   * Returns the horizontal rules whose code values (1, 2, 4, 8 and 32) are set in `mask`, or `universal` when none is.
   * `fromMaskH(15)` gives `Equal Character | Underscore | Hierarchy | Opposite Pair`.
   */
  def fromMaskH(mask: Int): SmushingRule[Horizontal] =
    Seq(equalCharacter, underscore, hierarchy, oppositePair, bigX, hardblank)
      .filter(m => (m.mask & mask & (1 + 2 + 4 + 8 + 16 + 32)) != 0)
      .fold(universal)(_ | _)

  /**
   * Returns the vertical rules whose code values (256, 512, 1024, 2048 and 4096) are set in `mask`, or `universal` when
   * none is.
   */
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

/** Decodes [[Layouts]] from the layout fields of a [[FigHeader]]. */
object Layouts {
  import Layout._
  import SmushingRule._

  /**
   * Decodes the layouts of a font from its header fields. When `fullLayoutMask` is present it alone decides: 128 gives
   * horizontal `Smushing` (with the rules from `SmushingRule.fromMaskH`), else 64 gives `Fitting`, else `FullWidth`;
   * 16384 gives vertical `Smushing` (rules from `SmushingRule.fromMaskV`), else 8192 gives `Fitting`, else `FullWidth`.
   *
   * Without it, `oldLayoutMask` sets only the horizontal layout (a negative value gives `FullWidth`, 0 `Fitting`, a
   * positive value `Smushing` with those rules) and the vertical layout is `FullWidth`. `fromHeaderMasks(15, None)`
   * gives `Layouts(Smushing(Equal Character | Underscore | Hierarchy | Opposite Pair), FullWidth)`.
   */
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
