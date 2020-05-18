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
sealed trait HelpDoc {
  def toPlaintext(columnWidth: Int = 100, color: Boolean = true): String = ???
  def toJson: String                                                     = ???
  def toHtml: String                                                     = ???
}

object HelpDoc {
  case object Empty                                                                         extends HelpDoc
  final case class Body(header: Option[Block], content: List[Block], footer: Option[Block]) extends HelpDoc

  sealed trait Block {
    def toPlaintext(columnWidth: Int = 100, color: Boolean = true): String = ???
    def toJson: String                                                     = ???
    def toHtml: String                                                     = ???
  }
  object Block {
    final case class Header(value: Span, level: Int) extends Block
    final case class Paragraph(value: Span)          extends Block
    final case class DescriptionList(definitions: List[(Span, Block)])
    final case class Enumeration(elements: List[Block])

    def paragraph(value: String): Block = Block.Paragraph(Span.Text(value))
  }

  sealed trait Span {
    def toPlaintext(columnWidth: Int = 100, color: Boolean = true): String = ???
    def toJson: String                                                     = ???
    def toHtml: String                                                     = ???
  }
  object Span {
    final case class Text(value: String)               extends Span
    final case class Code(value: Span)                 extends Span
    final case class Error(value: Span)                extends Span
    final case class Weak(value: Span)                 extends Span
    final case class Strong(value: Span)               extends Span
    final case class URI(value: java.net.URI)          extends Span
    final case class Sequence(left: Span, right: Span) extends Span
  }
}
