package zio.cli

/**
 * A `HelpDoc` models the full documentation for a command-line application.
 *
 * `HelpDoc` is composed of optional header and footers, and in-between, a o
 * list of HelpDoc-level content items.
 *
 * HelpDoc-level content items, in turn, can be headers, paragraphs, description
 * lists, and enumerations.
 *
 * A `HelpDoc` can be converted into plaintext, JSON, and HTML.
 */
sealed trait HelpDoc { self =>
  import HelpDoc._
  import scala.Console

  def +(that: HelpDoc): HelpDoc =
    (self, that) match {
      case (self, that) if self.isEmpty                                    => that
      case (self, that) if (that.isEmpty)                                  => this
      case (HelpDoc.DescriptionList(left), HelpDoc.DescriptionList(right)) => HelpDoc.DescriptionList(left ++ right)
      case (HelpDoc.Enumeration(left), HelpDoc.Enumeration(right))         => HelpDoc.Enumeration(left ++ right)
      case _                                                               => HelpDoc.Sequence(self, that)
    }

  def |(that: HelpDoc): HelpDoc = if (self.isEmpty) that else self

  def isHeader: Boolean =
    self match {
      case HelpDoc.Empty                 => true
      case HelpDoc.DescriptionList(xs)   => true
      case HelpDoc.Sequence(left, right) => left.isDescriptionList
      case HelpDoc.Enumeration(xs)       => false
      case _                             => false
    }

  def isParagraph: Boolean =
    self match {
      case HelpDoc.Header(_, _)          => true
      case HelpDoc.Sequence(left, right) => left.isParagraph
      case _                             => false
    }

  def isDescriptionList: Boolean =
    self match {
      case HelpDoc.DescriptionList(xs)   => true
      case HelpDoc.Sequence(left, right) => left.isDescriptionList
      case _                             => false
    }

  def isEmpty: Boolean =
    self match {
      case HelpDoc.Empty                 => true
      case HelpDoc.DescriptionList(xs)   => xs.forall(_._2.isEmpty)
      case HelpDoc.Sequence(left, right) => left.isEmpty && right.isEmpty
      case HelpDoc.Enumeration(xs)       => xs.forall(_.isEmpty)
      case _                             => false
    }

  def isSequence: Boolean =
    self match {
      case HelpDoc.Sequence(_, _) => true
      case _                      => false
    }

  def mapDescriptionList(f: (HelpDoc.Span, HelpDoc) => (HelpDoc.Span, HelpDoc)): HelpDoc =
    self match {
      case HelpDoc.DescriptionList(list) => HelpDoc.DescriptionList(list.map(f.tupled))
      case x                             => x
    }

  def toPlaintext(columnWidth: Int = 100, color: Boolean = true): String = {
    val _ = color

    val writer     = DocWriter(0, columnWidth)
    var uppercase  = false
    var styles     = List.empty[String]
    var lastStyle  = Console.RESET
    var printedSep = 0

    def setStyle(style: String): Unit = styles = style :: styles

    def currentStyle(): String = styles.headOption.getOrElse(Console.RESET)

    def resetStyle(): Unit = styles = styles.drop(1)

    def renderText(text: String): Unit =
      renderSpan(Span.text(text))

    def renderNewline(): Unit =
      if (printedSep < 2) {
        printedSep += 1
        val _ = writer.append("\n")
      }

    def clearSep() = printedSep = 0

    def renderHelpDoc(helpDoc: HelpDoc): Unit =
      helpDoc match {
        case Empty =>
        case HelpDoc.Header(value, level) =>
          writer.unindent()
          renderNewline()
          uppercase = true
          setStyle(Console.BOLD)
          renderSpan(value)
          resetStyle()
          uppercase = false
          renderNewline()
          writer.indent(4)

        case HelpDoc.Paragraph(value) =>
          renderSpan(value)
          renderNewline()

        case HelpDoc.DescriptionList(definitions) =>
          definitions.zipWithIndex.foreach {
            case ((span, helpDoc), index) =>
              setStyle(Console.BOLD)
              renderSpan(span)
              resetStyle()
              renderNewline()
              writer.indent(4)
              renderHelpDoc(helpDoc)
              writer.unindent()
              renderNewline()
          }

        case HelpDoc.Enumeration(elements) =>
          elements.zipWithIndex.foreach {
            case (helpDoc, index) =>
              writer.indent(2)
              renderText("- ")
              renderHelpDoc(helpDoc)
              writer.unindent()
              renderNewline()
          }

        case HelpDoc.Sequence(left, right) =>
          renderHelpDoc(left)
          renderNewline()
          renderHelpDoc(right)
      }

    def renderSpan(span: Span): Unit = {
      clearSep()
      val _ = span match {
        case Span.Text(value) =>
          if (color && (lastStyle != currentStyle())) {
            writer.append(currentStyle())
            lastStyle = currentStyle()
          }

          writer.append(if (uppercase) value.toUpperCase() else value)

        case Span.Code(value) =>
          setStyle(Console.WHITE)
          writer.append(value)
          resetStyle()

        case Span.Error(value) =>
          setStyle(Console.RED)
          renderSpan(value)
          resetStyle()

        case Span.Weak(value) =>
          setStyle(Console.BOLD)
          renderSpan(value)
          resetStyle()

        case Span.Strong(value) =>
          setStyle(Console.BOLD)
          renderSpan(value)
          resetStyle()

        case Span.URI(value) =>
          setStyle(Console.UNDERLINED)
          renderSpan(Span.text(value.toASCIIString()))
          resetStyle()

        case Span.Sequence(left, right) =>
          renderSpan(left)
          renderSpan(right)
      }
    }

    renderHelpDoc(this)

    writer.toString() + (if (color) Console.RESET else "")
  }
}
object HelpDoc {
  case object Empty                                                    extends HelpDoc
  final case class Header(value: Span, level: Int)                     extends HelpDoc
  final case class Paragraph(value: Span)                              extends HelpDoc
  final case class DescriptionList(definitions: List[(Span, HelpDoc)]) extends HelpDoc
  final case class Enumeration(elements: List[HelpDoc])                extends HelpDoc
  final case class Sequence(left: HelpDoc, right: HelpDoc)             extends HelpDoc

  def blocks(bs: Iterable[HelpDoc]): HelpDoc =
    if (bs.isEmpty) HelpDoc.Empty else blocks(bs.head, bs.tail.toSeq: _*)

  def blocks(helpDoc: HelpDoc, helpDocs0: HelpDoc*): HelpDoc =
    helpDocs0.foldLeft(helpDoc)(_ + _)

  def descriptionList(definitions: (Span, HelpDoc)*): HelpDoc = HelpDoc.DescriptionList(definitions.toList)

  val empty: HelpDoc = Empty

  def enumeration(elements: HelpDoc*): HelpDoc = HelpDoc.Enumeration(elements.toList)

  def h1(t: String): HelpDoc  = h1(Span.text(t))
  def h1(span: Span): HelpDoc = HelpDoc.Header(span, 1)

  def h2(t: String): HelpDoc  = h2(Span.text(t))
  def h2(span: Span): HelpDoc = HelpDoc.Header(span, 2)

  def h3(t: String): HelpDoc  = h3(Span.text(t))
  def h3(span: Span): HelpDoc = HelpDoc.Header(span, 3)

  def p(t: String): HelpDoc  = HelpDoc.Paragraph(Span.text(t))
  def p(span: Span): HelpDoc = HelpDoc.Paragraph(span)

  sealed trait Span {
    def +(that: Span): Span = Span.Sequence(this, that)

    def size: Int
  }
  object Span {
    final case class Text(value: String) extends Span {
      def size = value.length
    }
    final case class Code(value: String) extends Span {
      def size = value.length
    }
    final case class Error(value: Span) extends Span {
      def size = value.size
    }
    final case class Weak(value: Span) extends Span {
      def size = value.size
    }
    final case class Strong(value: Span) extends Span {
      def size = value.size
    }
    final case class URI(value: java.net.URI) extends Span {
      def size = value.toString().length
    }
    final case class Sequence(left: Span, right: Span) extends Span {
      def size = left.size + right.size
    }

    def text(t: String): Span                  = Span.Text(t)
    def spans(span: Span, spans0: Span*): Span = spans(span :: spans0.toList)

    def spans(spans: Iterable[Span]): Span = spans.toList.foldLeft(text("")) {
      case (span, s) => Span.Sequence(span, s)
    }
    def error(span: Span): Span = Span.Error(span)
    def error(t: String): Span  = Span.Error(text(t))

    def code(t: String): Span = Span.Code(t)

    def weak(span: Span): Span = Span.Weak(span)
    def weak(t: String): Span  = Span.Weak(text(t))

    def strong(span: Span): Span = Span.Strong(span)
    def strong(t: String): Span  = Span.Strong(text(t))

    def uri(uri: java.net.URI): Span = Span.URI(uri)

    def space: Span = text(" ")
  }
}

private[cli] class DocWriter(stringBuilder: StringBuilder, startOffset: Int, columnWidth: Int) {
  private var marginStack: List[Int] = List(startOffset)

  def append(s: String): DocWriter = {
    if (s.length == 0) this
    else
      DocWriter.splitNewlines(s) match {
        case None =>
          if (currentColumn + s.length > columnWidth) {
            val remainder = columnWidth - currentColumn

            val lastSpace = {
              val lastSpace = s.take(remainder + 1).lastIndexOf(' ')

              if (lastSpace == -1) remainder else lastSpace
            }

            val before = s.take(lastSpace)
            val after  = s.drop(lastSpace).dropWhile(_ == ' ')

            append(before)
            append("\n")
            append(after)
          } else {
            val padding = currentMargin - currentColumn
            if (padding > 0) {
              stringBuilder.append(DocWriter.margin(padding))
              currentColumn += padding
            }
            stringBuilder.append(s)
            currentColumn += s.length
          }
        case Some(pieces) =>
          pieces.zipWithIndex.foreach {
            case (piece, index) =>
              append(piece)

              stringBuilder.append("\n")
              currentColumn = 0
          }
      }

    this
  }

  def currentMargin: Int = marginStack.sum

  var currentColumn: Int = startOffset

  def indent(adjust: Int): Unit = marginStack = adjust :: marginStack

  override def toString(): String = stringBuilder.toString()

  def unindent(): Unit = marginStack = marginStack.drop(1)
}
private[cli] object DocWriter {
  private def margin(n: Int): String = if (n <= 0) "" else List.fill(n)(" ").mkString
  def splitNewlines(s: String): Option[Array[String]] = {
    val count = s.count(_ == '\n')

    if (count == 0) None
    else
      Some {
        val size = if (count == s.length) count else count + 1

        val array = Array.ofDim[String](size)

        var i = 0
        var j = 0
        while (i < s.length) {
          val search   = s.indexOf('\n', i)
          val endIndex = if (search == -1) s.length else search

          array(j) = s.substring(i, endIndex)

          i = endIndex + 1
          j = j + 1
        }
        if (j < array.length) array(j) = ""

        array
      }
  }

  def apply(startOffset: Int, columnWidth: Int): DocWriter = {
    val builder = new StringBuilder
    builder.append(margin(startOffset))
    new DocWriter(builder, startOffset, if (columnWidth <= 0) startOffset + 1 else columnWidth)
  }
}
