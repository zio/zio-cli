package zio.cli

import zio.cli.HelpDoc.Span.Sequence

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
sealed trait HelpDoc {
  def toPlaintext(columnWidth: Int = 100, color: Boolean = true): String = ???
  def toJson: String                                                     = ???
  def toHtml: String                                                     = ???
}

object HelpDoc {
  object dsl {
    val empty: HelpDoc = Empty

    def body(content: Block): HelpDoc                   = Body(None, content, None)
    def body(content: Block, contents: Block*): HelpDoc = Body(None, blocks(content, contents: _*), None)

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

    def text(t: String): Span = Span.Text(t)
    def spans(span: Span, spans0: Span*): Span = spans(span :: spans0.toList)

    def spans(spans: Iterable[Span]):Span = spans.toList.foldLeft(text("")) {
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
  case object Empty                                                                   extends HelpDoc
  final case class Body(header: Option[Block], content: Block, footer: Option[Block]) extends HelpDoc

  sealed trait Block {
    def toPlaintext(columnWidth: Int = 100, color: Boolean = true): String = ???
    def toJson: String                                                     = ???
    def toHtml: String                                                     = ???
  }
  object Block {
    final case class Header(value: Span, level: Int)                   extends Block
    final case class Paragraph(value: Span)                            extends Block
    final case class DescriptionList(definitions: List[(Span, Block)]) extends Block
    final case class Enumeration(elements: List[Block])                extends Block
    final case class Sequence(left: Block, right: Block)               extends Block
  }

  sealed trait Span {
    def toPlaintext(columnWidth: Int = 100, color: Boolean = true): String = ???
    def toJson: String                                                     = ???
    def toHtml: String                                                     = ???

    def +(that: Span): Span = Sequence(this, that)
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
