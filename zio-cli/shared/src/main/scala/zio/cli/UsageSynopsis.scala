package zio.cli

import zio.cli.HelpDoc._

sealed trait UsageSynopsis { self =>
  final def +(that: UsageSynopsis): UsageSynopsis = UsageSynopsis.Sequence(self, that)

  final def enumerate: List[Span] = {
    import UsageSynopsis._

    def simplify(g: UsageSynopsis): UsageSynopsis =
      g match {
        case named @ Named(names, acceptedValues) =>
          if (render(named).head.isEmpty) None else named
        case Optional(None) => None
        case Optional(value) =>
          val syn = simplify(value)
          if (syn == None) None else Optional(syn)
        case Repeated(value) =>
          val syn = simplify(value)
          if (syn == None) None else Repeated(syn)
        case seq @ Sequence(left, right) =>
          val leftSyn  = simplify(left)
          val rightSyn = simplify(right)
          if (leftSyn == None) rightSyn else if (rightSyn == None) leftSyn else Sequence(leftSyn, rightSyn)
        case Alternation(left, right) =>
          val leftSyn  = simplify(left)
          val rightSyn = simplify(right)
          if (leftSyn == None) rightSyn else if (rightSyn == None) leftSyn else Sequence(leftSyn, rightSyn)
        case Mixed => Mixed
        case None  => None

      }

    def render(g: UsageSynopsis): List[Span] =
      g match {
        case Named(names, acceptedValues) =>
          val mainSpan =
            Span.text(names.mkString(", ")) + acceptedValues.fold(Span.empty)(c => Span.space + Span.text(c))
          if (names.length > 1) Span.text("(") + mainSpan + Span.text(")") :: Nil else mainSpan :: Nil

        case Optional(value) =>
          render(value).map(synopsis => Span.text("[") + synopsis + Span.text("]"))

        case Repeated(value) =>
          render(value).map(_ + Span.text("..."))

        case Sequence(left, right) =>
          val leftSpan  = render(left)
          val rightSpan = render(right)
          val separator = if (!leftSpan.isEmpty && !rightSpan.isEmpty) Span.space else Span.empty

          for {
            l <- leftSpan
            r <- rightSpan
          } yield l + separator + r

        case Alternation(left: Repeated, right) =>
          render(left) ++ render(right)

        case Alternation(left: Sequence, right) =>
          render(left) ++ render(right)

        case Alternation(left, right: Repeated) =>
          render(left) ++ render(right)

        case Alternation(left, right: Sequence) =>
          render(left) ++ render(right)

        case Alternation(left, right) =>
          for {
            l <- render(left)
            r <- render(right)
          } yield l + Span.text("|") + r

        case Mixed =>
          Span.text("<command>") :: Nil

        case None => Span.text("") :: Nil
      }

    render(simplify(self))
  }

  final def helpDoc: HelpDoc = enumerate match {
    case Nil         => HelpDoc.empty
    case head :: Nil => p(head)
    case list        => list.map(p).foldRight(HelpDoc.empty)(_ + _)
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
