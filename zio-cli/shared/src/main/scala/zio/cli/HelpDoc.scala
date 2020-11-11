package zio.cli

/**
 * A `HelpDoc` models the full documentation for a command-line application.
 *
 * `HelpDoc` is composed of optional header and footers, and in-between, a o
 * list of block-level content items.
 *
 * Block-level content items, in turn, can be headers, paragraphs, description
 * lists, and enumerations.
 *
 * A `HelpDoc` can be converted into plaintext, JSON, and HTML.
 */
final case class HelpDoc(blocks: List[HelpDoc.Block]) {
  import HelpDoc._
  import scala.Console

  def toPlaintext(columnWidth: Int = 100, color: Boolean = true): String = {
    val _ = color

    val writer         = DocWriter(0, columnWidth)
    var uppercase      = false
    var pendingNewline = false
    var styles         = List.empty[String]
    var lastStyle      = Console.RESET

    def setStyle(style: String): Unit = styles = style :: styles

    def currentStyle(): String = styles.headOption.getOrElse(Console.RESET)

    def resetStyle(): Unit = styles = styles.drop(1)

    def renderText(text: String): Unit =
      renderSpan(dsl.text(text))

    def renderNewline(): Unit = renderText("\n")

    def forceNewlines(): Unit =
      if (pendingNewline) {
        renderNewline()
        pendingNewline = false
      }

    def scheduleNewline(): Unit = pendingNewline = true

    def renderBlock(block: Block): Unit = {
      forceNewlines()

      block match {
        case Block.Header(value, level) =>
          writer.unindent()
          renderNewline()
          uppercase = true
          setStyle(Console.BOLD)
          renderSpan(value)
          resetStyle()
          uppercase = false
          writer.indent(4)
          scheduleNewline()

        case Block.Paragraph(value) =>
          renderSpan(value)
          scheduleNewline()

        case Block.DescriptionList(definitions) =>
          definitions.zipWithIndex.foreach {
            case ((span, block), index) =>
              setStyle(Console.BOLD)
              renderSpan(span)
              resetStyle()
              writer.indent(4)
              renderNewline()
              renderBlock(block)
              writer.unindent()
              if (index != definitions.length - 1) {
                forceNewlines()
                renderNewline()
              }
          }
        case Block.Enumeration(elements) =>
          elements.zipWithIndex.foreach {
            case (block, index) =>
              writer.indent(2)
              renderText("- ")
              renderBlock(block)
              writer.unindent()

              if (index != elements.length - 1) forceNewlines()
          }

        case Block.Sequence(left, right) =>
          renderBlock(left)
          if (!right.isHeader) renderNewline()
          renderBlock(right)
      }
    }

    def renderSpan(span: Span): Unit = {
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
          renderSpan(dsl.text(value.toASCIIString()))
          resetStyle()

        case Span.Sequence(left, right) =>
          renderSpan(left)
          renderSpan(right)

        case Span.Space =>
          renderSpan(dsl.text(" "))
      }
    }

    blocks.foreach(renderBlock(_))

    writer.toString() + (if (color) Console.RESET else "")
  }
}
object HelpDoc {
  val empty: HelpDoc = HelpDoc(Nil)

  object dsl {
    def h1(t: String): Block  = h1(text(t))
    def h1(span: Span): Block = Block.Header(span, 1)

    def h2(t: String): Block  = h2(text(t))
    def h2(span: Span): Block = Block.Header(span, 2)

    def h3(t: String): Block  = h3(text(t))
    def h3(span: Span): Block = Block.Header(span, 3)

    def p(t: String): Block  = Block.Paragraph(text(t))
    def p(span: Span): Block = Block.Paragraph(span)

    def descriptionList(definitions: (Span, Block)*): Block = Block.DescriptionList(definitions.toList)

    def enumeration(elements: Block*): Block = Block.Enumeration(elements.toList)

    def blocks(bs: Iterable[Block]): Block =
      if (bs.isEmpty) Block.Paragraph(space) else blocks(bs.head, bs.tail.toSeq: _*)

    def blocks(block: Block, blocks0: Block*): Block =
      blocks0.foldLeft(block) {
        case (acc, b) => Block.Sequence(acc, b)
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

    def space: Span = Span.Space
  }

  sealed trait Block { self =>
    def isHeader: Boolean =
      self match {
        case Block.Header(_, _) => true
        case _                  => false
      }

    def isParagraph: Boolean =
      self match {
        case Block.Paragraph(_) => true
        case _                  => false
      }

    def isDescriptionList: Boolean =
      self match {
        case Block.DescriptionList(_) => true
        case _                        => false
      }

    def isEnumeration: Boolean =
      self match {
        case Block.Enumeration(_) => true
        case _                    => false
      }

    def isSequence: Boolean =
      self match {
        case Block.Sequence(_, _) => true
        case _                    => false
      }
  }
  object Block {
    final case class Header(value: Span, level: Int)                   extends Block
    final case class Paragraph(value: Span)                            extends Block
    final case class DescriptionList(definitions: List[(Span, Block)]) extends Block
    final case class Enumeration(elements: List[Block])                extends Block
    final case class Sequence(left: Block, right: Block)               extends Block
  }

  sealed trait Span {
    def +(that: Span): Span = Span.Sequence(this, that)
  }
  object Span {
    final case class Text(value: String)               extends Span
    final case class Code(value: String)               extends Span
    final case class Error(value: Span)                extends Span
    final case class Weak(value: Span)                 extends Span
    final case class Strong(value: Span)               extends Span
    final case class URI(value: java.net.URI)          extends Span
    final case class Sequence(left: Span, right: Span) extends Span
    case object Space                                  extends Span
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
            stringBuilder.append(s)
            currentColumn += s.length
          }
        case Some(pieces) =>
          pieces.zipWithIndex.foreach {
            case (piece, index) =>
              append(piece)

              stringBuilder.append("\n").append(DocWriter.margin(currentMargin))
              currentColumn = currentMargin
          }
      }

    this
  }

  def currentMargin: Int = marginStack.sum

  var currentColumn: Int = startOffset

  def indent(adjust: Int): Unit = marginStack = adjust :: marginStack

  def newline(): DocWriter = append("\n")

  override def toString(): String = stringBuilder.toString()

  def unindent(): Unit = marginStack = marginStack.drop(1)
}
private[cli] object DocWriter {
  private def margin(n: Int): String = List.fill(n)(" ").mkString
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
