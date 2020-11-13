package zio.cli

import zio.cli.HelpDoc._

sealed trait UsageSynopsis { self =>
  def +(that: UsageSynopsis): UsageSynopsis = UsageSynopsis.Sequence(self, that)

  def helpDoc: HelpDoc = {
    import UsageSynopsis._

    def render(g: UsageSynopsis): Span =
      g match {
        case Command(name) =>
          Span.strong(Span.text(name))

        case Option(name, valueType) =>
          Span.text(name) + valueType.fold(Span.text(""))(valueType => Span.text(" ") + Span.text(valueType))

        case Optional(value) =>
          Span.text("[") + render(value) + Span.text("]")

        case Repeated(value) =>
          render(value) + Span.text("...")

        case Argument(name) => Span.text(name)

        case Sequence(left, right) =>
          render(left) + Span.text(" ") + render(right)

        case Alternation(left, right) =>
          render(left) + Span.text("|") + render(right)

        case Mixed =>
          Span.text("<command> [<args>]")

        case None => Span.text("")
      }

    p(render(self))
  }
}
object UsageSynopsis {
  final case class Command(name: String)                                  extends UsageSynopsis
  final case class Option(name: String, valueType: scala.Option[String])  extends UsageSynopsis
  final case class Optional(value: UsageSynopsis)                         extends UsageSynopsis
  final case class Repeated(value: UsageSynopsis)                         extends UsageSynopsis
  final case class Argument(name: String)                                 extends UsageSynopsis
  final case class Sequence(left: UsageSynopsis, right: UsageSynopsis)    extends UsageSynopsis
  final case class Alternation(left: UsageSynopsis, right: UsageSynopsis) extends UsageSynopsis
  case object Mixed                                                       extends UsageSynopsis
  case object None                                                        extends UsageSynopsis
}
