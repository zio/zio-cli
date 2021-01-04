package figlet

import zio.Chunk

private[figlet] object FigFontParser {
  def parse(lines: Chunk[String]): Either[String, FigFont] = figFont.parse(TextSpan(lines))

  import ParseResult._

  type TextParser[+A] = Parser[TextSpan, TextSpan, A]
  type LineParser[+A] = Parser[LineSpan, LineSpan, A]

  final val token: LineParser[String] = Parser { r =>
    val n = r.chars.indexOf(' ', r.a) match {
      case -1 => r.size
      case i  => scala.math.min(r.size, i - r.a)
    }
    Ok(r.drop(n), r.chars.substring(r.a, r.a + n))
  }

  final val int = token.refine[Int](s =>
    s.toIntOption match {
      case Some(i) => Right(i)
      case None    => Left(_.error("Expected integer"))
    }
  )

  final val space = char(' ')
  final val line  = lines(1).map(_(0))

  final val figHeader = for {
    signature      <- chars(5)
    hardBlank      <- chars(1).map(_.head)
    charHeight     <- space ~> int
    baseline       <- space ~> int
    _              <- space ~> int // we have no use for maxLength
    oldLayoutMask  <- space ~> int
    commentLines   <- space ~> int
    rightToLeft    <- (space ~> int).?
    fullLayoutMask <- (space ~> int).?
  } yield FigHeader(
    signature = signature,
    hardBlank = hardBlank,
    charHeight = charHeight,
    baseline = baseline,
    commentLines = commentLines,
    oldLayoutMask = oldLayoutMask,
    rightToLeft = rightToLeft,
    fullLayoutMask = fullLayoutMask
  )

  def figChar(charHeight: Int): TextParser[FigChar] =
    for {
      lines        <- lines(charHeight)
      endMarker    = lines(0).last
      width        = lines(0).indexOf(endMarker.toInt)
      figCharLines = lines.map(FigCharLine.fromFullLine(width, _))
    } yield FigChar(figCharLines, width, charHeight)

  final val charTag = parseLine(
    token.refine(s =>
      try {
        Right(Integer.decode(s).toChar)
      } catch {
        case _: NumberFormatException => Left(_.error("Expected a char code"))
      }
    )
  )

  final val requiredChars: Seq[Char] = (32 until 127).map(_.toChar) ++ "ÄÖÜßäöü".toCharArray

  final val figFont = for {
    header        <- parseLine(figHeader)
    _             <- lines(header.commentLines)
    fc            = figChar(header.charHeight)
    requiredChars <- fc.rep(requiredChars.size).map(requiredChars.zip(_))
    taggedChars   <- (charTag ~ fc).*
  } yield FigFont(
    header,
    rightToLeft = header.rightToLeft.contains(1),
    layout = Layouts.fromHeaderMasks(header.oldLayoutMask, header.fullLayoutMask),
    chars = (requiredChars ++ taggedChars).toMap
  )

  final case class TextSpan(lines: Chunk[String], pos: Int = 0) {
    def size: Int                = lines.size
    def drop(n: Int): TextSpan   = TextSpan(lines.drop(n), pos + n)
    def error(e: => String): Err = Err(() => s"${pos + 1}:$e")
  }

  final case class LineSpan(chars: String, a: Int, b: Int) {
    def size: Int                = b - a
    def error(e: => String): Err = Err(() => s"${a + 1}:$e")
    def drop(n: Int): LineSpan   = LineSpan(chars, a + n, b)
  }

  object LineSpan {
    def apply(chars: String, a: Int = 0): LineSpan = LineSpan(chars, a, chars.length)
  }

  def lines(n: Int): TextParser[Chunk[String]] = Parser { r =>
    if (r.size >= n) Ok(r.drop(n), r.lines.take(n)) else r.error(s"EOF before ${n}th line")
  }

  def chars(n: Int): LineParser[String] = Parser { r =>
    if (r.size >= n) Ok(r.drop(n), r.chars.substring(r.a, r.a + n)) else r.error(s"EOL before ${n}th char")
  }

  def char(c: Char): LineParser[Unit] = Parser { r =>
    if (r.size == 0) r.error(s"'$c' expected, but found EOL")
    else if (r.chars.charAt(r.a) != c) r.error(s"'$c' expected, found '${r.chars.charAt(r.a)}'")
    else Ok(r.drop(1), ())
  }

  def parseLine[A](p: Parser[LineSpan, Any, A]): TextParser[A] =
    line.refine[A](l =>
      p(LineSpan(l)) match {
        case Ok(_, t) => Right(t)
        case Err(e)   => Left(_.error(e()))
      }
    )
}
