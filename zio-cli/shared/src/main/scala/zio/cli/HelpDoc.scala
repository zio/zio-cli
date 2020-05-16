package zio.cli

sealed trait HelpDoc {
  def toPlaintext(columnWidth: Int = 100, color: Boolean = true): String = ???
  def toJson: String                                                     = ???
  def toHtml: String                                                     = ???
}

object HelpDoc {
  case object Empty                                                                         extends HelpDoc
  final case class Body(header: Option[Block], content: List[Block], footer: Option[Block]) extends HelpDoc

  sealed trait Block
  object Block {
    final case class Header(value: Span, level: Int) extends Block
    final case class Paragraph(value: Span)          extends Block
    final case class DescriptionList(definitions: List[(Span, Block)])
    final case class Enumeration(elements: List[Block])
  }

  sealed trait Span
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
