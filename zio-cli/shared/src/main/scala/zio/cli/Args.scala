package zio.cli

import java.nio.file.{ Path => JPath }

import java.time.{
  Instant => JInstant,
  LocalDate => JLocalDate,
  LocalDateTime => JLocalDateTime,
  LocalTime => JLocalTime,
  MonthDay => JMonthDay,
  OffsetDateTime => JOffsetDateTime,
  OffsetTime => JOffsetTime,
  Period => JPeriod,
  Year => JYear,
  YearMonth => JYearMonth,
  ZoneId => JZoneId,
  ZoneOffset => JZoneOffset,
  ZonedDateTime => JZonedDateTime
}

import zio.IO
import zio.cli.HelpDoc.Span

sealed trait Args[+A] { self =>

  final def ++[That, A1 >: A](that: Args[That]): Args.Cons[A1, That] =
    Args.Cons(self, that)

  final def * : Args[List[A]] = Args.Variadic(self, None, None)

  def ??(that: String): Args[A]

  final def atLeast(min: Int): Args[List[A]] = Args.Variadic(self, Some(min), None)

  final def atMost(max: Int): Args[List[A]] = Args.Variadic(self, None, Some(max))

  final def between(min: Int, max: Int): Args[List[A]] = Args.Variadic(self, Some(min), Some(max))

  def helpDoc: HelpDoc

  def maxSize: Int

  def minSize: Int

  final def repeat: Args[List[A]] = self.*

  def synopsis: CLIGrammar

  def validate(args: List[String], opts: ParserOptions): IO[List[HelpDoc], (List[String], A)]
}

object Args {

  final case class Single[+A](pseudoName: String, primType: PrimType[A], description: Vector[String] = Vector())
      extends Args[A] {
    self =>
    def ??(that: String): Args[A] = copy(description = description :+ that)

    def helpDoc: HelpDoc =
      HelpDoc.DescriptionList(
        List(
          (Span.text("<") + Span.weak(pseudoName) + Span.text(">") + Span.text(": " + primType.typeName + "")) ->
            HelpDoc.blocks(description.map(HelpDoc.p(_)))
        )
      )

    def maxSize: Int = 1

    def minSize: Int = 1

    def synopsis: CLIGrammar = CLIGrammar.Argument("<" + pseudoName.toLowerCase + ": " + primType.typeName + ">")

    def validate(args: List[String], opts: ParserOptions): IO[List[HelpDoc], (List[String], A)] =
      args match {
        case head :: tail => primType.validate(head).bimap(text => HelpDoc.p(text) :: Nil, a => tail -> a)
        case Nil =>
          IO.fail(HelpDoc.p(s"Missing argument <${pseudoName}> of type ${primType.typeName}.") :: Nil)
      }
  }

  case object Empty extends Args[Unit] {
    def ??(that: String): Args[Unit] = Empty

    def helpDoc: HelpDoc = HelpDoc.Empty

    def maxSize: Int = 0

    def minSize: Int = 0

    def synopsis: CLIGrammar = CLIGrammar.None

    def validate(args: List[String], opts: ParserOptions): IO[List[HelpDoc], (List[String], Unit)] =
      IO.succeed((args, ()))
  }

  final case class Cons[+A, +B](head: Args[A], tail: Args[B]) extends Args[(A, B)] {
    def ??(that: String): Args[(A, B)] = Cons(head ?? that, tail ?? that)

    def helpDoc: HelpDoc = head.helpDoc + tail.helpDoc

    def maxSize: Int = head.maxSize + tail.maxSize

    def minSize: Int = head.minSize + tail.minSize

    def synopsis: CLIGrammar = head.synopsis + tail.synopsis

    def validate(args: List[String], opts: ParserOptions): IO[List[HelpDoc], (List[String], (A, B))] =
      for {
        tuple     <- head.validate(args, opts)
        (args, a) = tuple
        tuple     <- tail.validate(args, opts)
        (args, b) = tuple
      } yield (args, (a, b))
  }

  final case class Variadic[+A](value: Args[A], min: Option[Int], max: Option[Int]) extends Args[List[A]] {
    def ??(that: String): Args[List[A]] = Variadic(value ?? that, min, max)

    def synopsis: CLIGrammar = CLIGrammar.Repeated(value.synopsis)

    def helpDoc: HelpDoc = value.helpDoc.mapDescriptionList {
      case (span, block) =>
        val newSpan = span + Span.text(" ") + Span.text(
          if (max.isDefined) s" ${minSize} - ${maxSize}" else s"${minSize}+"
        )
        val newBlock =
          block +
            HelpDoc.p(
              if (max.isDefined)
                s"This argument must be repeated at least ${minSize} times and may be repeated up to ${maxSize} times."
              else s"This argument must be repeated at least ${minSize} times."
            )

        (newSpan, newBlock)
    }

    def maxSize: Int = max.getOrElse(Int.MaxValue / 2) * value.maxSize

    def minSize: Int = min.getOrElse(0) * value.minSize

    def validate(args: List[String], opts: ParserOptions): IO[List[HelpDoc], (List[String], List[A])] = {
      val min1 = min.getOrElse(0)
      val max1 = max.getOrElse(Int.MaxValue)

      def loop(args: List[String], acc: List[A]): IO[List[HelpDoc], (List[String], List[A])] =
        if (acc.length >= max1) IO.succeed(args -> acc)
        else
          value
            .validate(args, opts)
            .foldM(
              failure => if (acc.length >= min1) IO.succeed(args -> acc) else IO.fail(failure),
              { case (args, a) => loop(args, a :: acc) }
            )

      loop(args, Nil).map { case (args, list) => (args, list.reverse) }
    }
  }

  def bool(name: String): Args[Boolean] = Single(name, PrimType.Boolean)

  def file(name: String, exists: Exists = Exists.Either): Args[JPath] =
    Single(name, PrimType.Path(PathType.File, exists))

  def directory(name: String, exists: Exists = Exists.Either): Args[JPath] =
    Single(name, PrimType.Path(PathType.Directory, exists))

  def text(name: String): Args[String] =
    Single(name, PrimType.Text)

  def decimal(name: String): Args[BigDecimal] =
    Single(name, PrimType.Decimal)

  def integer(name: String): Args[BigInt] =
    Single(name, PrimType.Integer)

  def instant(name: String): Args[JInstant] =
    Single(name, PrimType.Instant)

  def localDate(name: String): Args[JLocalDate] =
    Single(name, PrimType.LocalDate)

  def localDateTime(name: String): Args[JLocalDateTime] =
    Single(name, PrimType.LocalDateTime)

  def localTime(name: String): Args[JLocalTime] =
    Single(name, PrimType.LocalTime)

  def monthDay(name: String): Args[JMonthDay] =
    Single(name, PrimType.MonthDay)

  val none: Args[Unit] = Empty

  def offsetDateTime(name: String): Args[JOffsetDateTime] =
    Single(name, PrimType.OffsetDateTime)

  def offsetTime(name: String): Args[JOffsetTime] =
    Single(name, PrimType.OffsetTime)

  def period(name: String): Args[JPeriod] =
    Single(name, PrimType.Period)

  def year(name: String): Args[JYear] =
    Single(name, PrimType.Year)

  def yearMonth(name: String): Args[JYearMonth] =
    Single(name, PrimType.YearMonth)

  def zonedDateTime(name: String): Args[JZonedDateTime] =
    Single(name, PrimType.ZonedDateTime)

  def zoneId(name: String): Args[JZoneId] =
    Single(name, PrimType.ZoneId)

  def zoneOffset(name: String): Args[JZoneOffset] =
    Single(name, PrimType.ZoneOffset)
}
