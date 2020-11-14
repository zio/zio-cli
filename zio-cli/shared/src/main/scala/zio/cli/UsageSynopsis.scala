package zio.cli

import zio.cli.HelpDoc._

sealed trait UsageSynopsis { self =>
  def +(that: UsageSynopsis): UsageSynopsis = UsageSynopsis.Sequence(self, that)

  def helpDoc: HelpDoc = {
    import UsageSynopsis._

    def render(g: UsageSynopsis): Span =
      g match {
        case Named(name, values) =>
          Span.text("<" + name + ">") + values.fold(Span.text(""))(v => Span.text(" ") + Span.text(v))

        case Optional(value) =>
          Span.text("[") + render(value) + Span.text("]")

        case Repeated(value) =>
          render(value) + Span.text("...")

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
  final case class Named(name: String, values: scala.Option[String])      extends UsageSynopsis
  final case class Optional(value: UsageSynopsis)                         extends UsageSynopsis
  final case class Repeated(value: UsageSynopsis)                         extends UsageSynopsis
  final case class Sequence(left: UsageSynopsis, right: UsageSynopsis)    extends UsageSynopsis
  final case class Alternation(left: UsageSynopsis, right: UsageSynopsis) extends UsageSynopsis
  case object Mixed                                                       extends UsageSynopsis
  case object None                                                        extends UsageSynopsis
}
