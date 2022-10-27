package zio.cli

import zio.cli.HelpDoc._

sealed trait UsageSynopsis { self =>
  final def +(that: UsageSynopsis): UsageSynopsis = UsageSynopsis.Sequence(self, that)

  final def helpDoc: HelpDoc = {
    import UsageSynopsis._

    def render(g: UsageSynopsis): Span =
      g match {
        case Named(names, acceptedValues) =>
          val mainSpan =
            Span.text(names.mkString(", ")) + acceptedValues.fold(Span.empty)(c => Span.space + Span.text(c))
          if (names.length > 1) Span.text("(") + mainSpan + Span.text(")") else mainSpan

        case Optional(value) =>
          Span.text("[") + render(value) + Span.text("]")

        case Repeated(value) =>
          render(value) + Span.text("...")

        case Sequence(left, right) =>
          val leftSpan  = render(left)
          val rightSpan = render(right)
          val separator = if (!leftSpan.isEmpty && !rightSpan.isEmpty) Span.space else Span.empty

          leftSpan + separator + rightSpan

        case Alternation(left, right) =>
          render(left) + Span.text("|") + render(right)

        case Mixed =>
          Span.text("<command>")

        case None => Span.text("")
      }

    p(render(self))
  }

  final def optional: UsageSynopsis = UsageSynopsis.Optional(self)
}
object UsageSynopsis {
  final case class Named(names: List[String], acceptedValues: scala.Option[String]) extends UsageSynopsis
  final case class Optional(value: UsageSynopsis)                                   extends UsageSynopsis
  final case class Repeated(value: UsageSynopsis)                                   extends UsageSynopsis
  final case class Sequence(left: UsageSynopsis, right: UsageSynopsis)              extends UsageSynopsis
  final case class Alternation(left: UsageSynopsis, right: UsageSynopsis)           extends UsageSynopsis
  case object Mixed                                                                 extends UsageSynopsis
  case object None                                                                  extends UsageSynopsis
}
