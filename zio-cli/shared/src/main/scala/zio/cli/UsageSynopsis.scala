package zio.cli

import zio.cli.HelpDoc._

sealed trait UsageSynopsis { self =>
  def +(that: UsageSynopsis): UsageSynopsis = UsageSynopsis.Sequence(self, that)

  def helpDoc: HelpDoc = {
    import UsageSynopsis._

    def render(g: UsageSynopsis): Span =
      g match {
        case Named(name, choices) =>
          Span.text(name) + Span.text(choices.fold("")(v => if (v.length < 10) " " + v else ""))

        case Optional(value) =>
          Span.text("[") + render(value) + Span.text("]")

        case Repeated(value) =>
          render(value) + Span.text("...")

        case Sequence(left, right) =>
          val leftSpan  = render(left)
          val rightSpan = render(right)

          if (leftSpan.isEmpty && rightSpan.isEmpty) Span.empty
          else if (leftSpan.isEmpty) rightSpan
          else if (rightSpan.isEmpty) leftSpan
          else leftSpan + Span.space + rightSpan

        case Alternation(left, right) => // TODO do we really need this?
          render(left) + Span.text("|") + render(right)

        case Mixed =>
          Span.text("<command> [<args>]") // TODO should we always display [<args>] here?

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
