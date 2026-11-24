package zio.cli

import zio.cli.HelpDoc._

/**
 * The usage line of a command or a parameter, such as `grep [(-v, --verbose)] <file>`: names, optional parts in square brackets, repeated parts followed by `...`, and alternatives. The help screen of a [[CliApp]] shows it in its usage section.
 */
sealed trait UsageSynopsis { self =>
  /**
   * Puts `that` after this synopsis, separated by a space.
   */
  final def +(that: UsageSynopsis): UsageSynopsis = UsageSynopsis.Sequence(self, that)

  /**
   * Renders this synopsis as one or more lines of text. An alternative whose side is a sequence or a repetition gives one line per side; other alternatives are shown on one line as `a|b`. `config.showAllNames` and `config.showTypes` decide whether every name of an option and its value placeholder are shown. Parts that render as empty text are dropped.
   */
  final def enumerate(config: CliConfig): List[Span] = {
    import UsageSynopsis._

    def simplify(g: UsageSynopsis): UsageSynopsis =
      g match {
        case named @ Named(_, _) =>
          if (render(named).head.isEmpty) None else named
        case Optional(None)  => None
        case Optional(value) =>
          val syn = simplify(value)
          if (syn == None) None else Optional(syn)
        case Repeated(value) =>
          val syn = simplify(value)
          if (syn == None) None else Repeated(syn)
        case Sequence(left, right) =>
          val leftSyn  = simplify(left)
          val rightSyn = simplify(right)
          if (leftSyn == None) rightSyn else if (rightSyn == None) leftSyn else Sequence(leftSyn, rightSyn)
        case Alternation(left, right) =>
          val leftSyn  = simplify(left)
          val rightSyn = simplify(right)
          if (leftSyn == None) rightSyn else if (rightSyn == None) leftSyn else Alternation(leftSyn, rightSyn)
        case Mixed => Mixed
        case None  => None

      }

    def render(g: UsageSynopsis): List[Span] =
      g match {
        case Named(names, acceptedValues) =>
          val typeInfo =
            if (config.showTypes) acceptedValues.fold(Span.empty)(c => Span.space + Span.text(c))
            else Span.empty
          val namesToShow =
            if (config.showAllNames) names
            else if (names.length > 1)
              names
                .filter(_.startsWith("--"))
                .headOption
                .map(_ :: Nil)
                .getOrElse(names)
            else names
          val nameInfo = Span.text(namesToShow.mkString(", "))
          if (config.showAllNames && names.length > 1)
            Span.text("(") + nameInfo + typeInfo + Span.text(")") :: Nil
          else
            nameInfo + typeInfo :: Nil

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

  /**
   * Renders this synopsis with `CliConfig.default`, one paragraph per line.
   */
  final def helpDoc: HelpDoc = enumerate(CliConfig.default) match {
    case Nil         => HelpDoc.empty
    case head :: Nil => p(head)
    case list        => list.map(p).foldRight(HelpDoc.empty)(_ + _)
  }

  /**
   * Marks this synopsis as optional; it is shown in square brackets.
   */
  final def optional: UsageSynopsis = UsageSynopsis.Optional(self)
}
/**
 * The cases of [[UsageSynopsis]].
 */
object UsageSynopsis {
  /**
   * An option or an argument: its `names` and, if any, the values it accepts (a placeholder such as `<text>`, or choices such as `red | blue`). With more than one name, it is shown in parentheses, the names separated by commas.
   */
  final case class Named(names: List[String], acceptedValues: scala.Option[String]) extends UsageSynopsis
  /**
   * A part shown in square brackets.
   */
  final case class Optional(value: UsageSynopsis)                                   extends UsageSynopsis
  /**
   * A part followed by `...`.
   */
  final case class Repeated(value: UsageSynopsis)                                   extends UsageSynopsis
  /**
   * Two parts, separated by a space.
   */
  final case class Sequence(left: UsageSynopsis, right: UsageSynopsis)              extends UsageSynopsis
  /**
   * Two alternatives, shown as `left|right`, or on separate lines when a side is a sequence or a repetition.
   */
  final case class Alternation(left: UsageSynopsis, right: UsageSynopsis)           extends UsageSynopsis
  /**
   * A choice between commands, shown as `<command>`.
   */
  case object Mixed                                                                 extends UsageSynopsis
  /**
   * Nothing; it renders as empty text.
   */
  case object None                                                                  extends UsageSynopsis
}
