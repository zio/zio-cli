package zio.cli

import zio.cli.HelpDoc._

sealed trait CLIGrammar { self =>
  def +(that: CLIGrammar): CLIGrammar = CLIGrammar.Sequence(self, that)

  def helpDoc: HelpDoc = {
    import CLIGrammar._

    def render(g: CLIGrammar): Span =
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

        case Mixed =>
          Span.text("<command> [<args>]")

        case None => Span.text("")
      }

    p(render(self))
  }
}
object CLIGrammar {
  final case class Command(name: String)                                 extends CLIGrammar
  final case class Option(name: String, valueType: scala.Option[String]) extends CLIGrammar
  final case class Optional(value: CLIGrammar)                           extends CLIGrammar
  final case class Repeated(value: CLIGrammar)                           extends CLIGrammar
  final case class Argument(name: String)                                extends CLIGrammar
  final case class Sequence(left: CLIGrammar, right: CLIGrammar)         extends CLIGrammar
  case object Mixed                                                      extends CLIGrammar
  case object None                                                       extends CLIGrammar
}
