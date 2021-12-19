package zio.cli

import java.nio.file.{Path => JPath}
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
import zio.{IO, Zippable}
import zio.cli.HelpDoc.Span

/**
 * A `Args` represents arguments that can be passed to a command-line application.
 */
sealed trait Args[+A] { self =>

  final def ++[B](that: Args[B])(implicit zippable: Zippable[A, B]): Args[zippable.Out] =
    Args.Both(self, that).map { case (a, b) => zippable.zip(a, b) }

  final def * : Args[List[A]] = Args.Variadic(self, None, None)

  def ??(that: String): Args[A]

  final def atLeast(min: Int): Args[List[A]] = Args.Variadic(self, Some(min), None)

  final def atMost(max: Int): Args[List[A]] = Args.Variadic(self, None, Some(max))

  final def between(min: Int, max: Int): Args[List[A]] = Args.Variadic(self, Some(min), Some(max))

  def helpDoc: HelpDoc

  final def map[B](f: A => B): Args[B] = Args.Map(self, (a: A) => Right(f(a)))

  final def mapOrFail[B](f: A => Either[HelpDoc, B]): Args[B] =
    Args.Map(self, (a: A) => f(a))

  final def mapTry[B](f: A => B): Args[B] =
    self.mapOrFail((a: A) => scala.util.Try(f(a)).toEither.left.map(e => HelpDoc.p(e.getMessage())))

  def maxSize: Int

  def minSize: Int

  final def repeat: Args[List[A]] = self.*

  final def repeat1[A1 >: A]: Args[::[A1]] = Args.Variadic(self, Some(1), None).map {
    case head :: tail => ::(head, tail)
    case Nil          => throw new IllegalStateException("Args.Variadic is not respecting the minimum.")
  }

  def synopsis: UsageSynopsis

  def validate(args: List[String], conf: CliConfig): IO[HelpDoc, (List[String], A)]
}

object Args {

  final case class Single[+A](pseudoName: Option[String], primType: PrimType[A], description: HelpDoc = HelpDoc.Empty)
      extends Args[A] {
    self =>
    def ??(that: String): Args[A] = copy(description = description + HelpDoc.p(that))

    def helpDoc: HelpDoc =
      HelpDoc.DescriptionList(
        List(
          Span.weak(name) ->
            (description | HelpDoc.p(primType.helpDoc))
        )
      )

    def maxSize: Int = 1

    def minSize: Int = 1

    def synopsis: UsageSynopsis = UsageSynopsis.Named(name, primType.choices)

    def validate(args: List[String], conf: CliConfig): IO[HelpDoc, (List[String], A)] =
      args match {
        case head :: tail => primType.validate(Some(head), conf).mapBoth(text => HelpDoc.p(text), a => tail -> a)
        case Nil =>
          val msg = (pseudoName, primType.choices) match {
            case (Some(pseudoName), Some(choices)) => s"Missing argument <$pseudoName> with values $choices."
            case (Some(pseudoName), _)             => s"Missing argument <$pseudoName>."
            case (None, Some(choices))             => s"Missing argument ${primType.typeName} with values $choices."
            case (None, None)                      => s"Missing argument ${primType.typeName}."
          }
          IO.fail(HelpDoc.p(msg))
      }

    private def name: String = "<" + pseudoName.getOrElse(primType.typeName) + ">"
  }

  case object Empty extends Args[Unit] {
    def ??(that: String): Args[Unit] = Empty

    def helpDoc: HelpDoc = HelpDoc.Empty

    def maxSize: Int = 0

    def minSize: Int = 0

    def synopsis: UsageSynopsis = UsageSynopsis.None

    def validate(args: List[String], conf: CliConfig): IO[HelpDoc, (List[String], Unit)] =
      IO.succeed((args, ()))
  }

  final case class Both[+A, +B](head: Args[A], tail: Args[B]) extends Args[(A, B)] {
    def ??(that: String): Args[(A, B)] = Both(head ?? that, tail ?? that)

    def helpDoc: HelpDoc = head.helpDoc + tail.helpDoc

    def maxSize: Int = head.maxSize + tail.maxSize

    def minSize: Int = head.minSize + tail.minSize

    def synopsis: UsageSynopsis = head.synopsis + tail.synopsis

    def validate(args: List[String], conf: CliConfig): IO[HelpDoc, (List[String], (A, B))] =
      for {
        tuple    <- head.validate(args, conf)
        (args, a) = tuple
        tuple    <- tail.validate(args, conf)
        (args, b) = tuple
      } yield (args, (a, b))
  }

  final case class Variadic[+A](value: Args[A], min: Option[Int], max: Option[Int]) extends Args[List[A]] {
    def ??(that: String): Args[List[A]] = Variadic(value ?? that, min, max)

    def synopsis: UsageSynopsis = UsageSynopsis.Repeated(value.synopsis)

    def helpDoc: HelpDoc = value.helpDoc.mapDescriptionList { case (span, block) =>
      val newSpan = span + Span.text(
        if (max.isDefined) s" $minSize - $maxSize" else if (minSize == 0) "..." else s" $minSize+"
      )
      val newBlock =
        block +
          HelpDoc.p(
            if (max.isDefined)
              s"This argument must be repeated at least $minSize times and may be repeated up to $maxSize times."
            else if (minSize == 0) "This argument may be repeated zero or more times."
            else s"This argument must be repeated at least $minSize times."
          )

      (newSpan, newBlock)
    }

    def maxSize: Int = max.getOrElse(Int.MaxValue / 2) * value.maxSize

    def minSize: Int = min.getOrElse(0) * value.minSize

    def validate(args: List[String], conf: CliConfig): IO[HelpDoc, (List[String], List[A])] = {
      val min1 = min.getOrElse(0)
      val max1 = max.getOrElse(Int.MaxValue)

      def loop(args: List[String], acc: List[A]): IO[HelpDoc, (List[String], List[A])] =
        if (acc.length >= max1) IO.succeed(args -> acc)
        else
          value
            .validate(args, conf)
            .foldZIO(
              failure => if (acc.length >= min1 && args.isEmpty) IO.succeed(args -> acc) else IO.fail(failure),
              { case (args, a) => loop(args, a :: acc) }
            )

      loop(args, Nil).map { case (args, list) => (args, list.reverse) }
    }
  }

  final case class Map[A, B](value: Args[A], f: A => Either[HelpDoc, B]) extends Args[B] {
    def ??(that: String): Args[B] = Map(value ?? that, f)

    def helpDoc: HelpDoc = value.helpDoc

    def maxSize: Int = value.maxSize

    def minSize: Int = value.minSize

    def synopsis: UsageSynopsis = value.synopsis

    def validate(args: List[String], conf: CliConfig): IO[HelpDoc, (List[String], B)] =
      value.validate(args, conf).flatMap { case (r, a) =>
        f(a) match {
          case Left(value)  => IO.fail(value)
          case Right(value) => IO.succeed((r, value))
        }
      }
  }

  /**
   * Creates a boolean argument with a custom argument name
   *
   * @param name Argument name
   * @return Boolean argument
   */
  def bool(name: String): Args[Boolean] = Single(Some(name), PrimType.Bool(None))

  /**
   * Creates a boolean argument with boolean as argument name
   */
  val bool: Args[Boolean] = Single(None, PrimType.Bool(None))

  /**
   * Creates a enumeration argument with a custom argument name
   *
   * @param name Argument name
   * @param cases Enum cases
   * @return Enumeration argument
   */
  def enumeration[A](name: String)(cases: (String, A)*): Args[A] =
    Single(Some(name), PrimType.Enumeration(cases: _*))

  /**
   * Creates a enumeration argument with 'choice' as argument name
   *
   * @param cases Enum cases
   * @return Enumeration argument
   */
  def enumeration[A](cases: (String, A)*): Args[A] =
    Single(None, PrimType.Enumeration(cases: _*))

  /**
   * Creates a file argument with a custom argument name
   *
   * @param name Argument name
   * @param exists Yes if path is expected to exists, No otherwise or Either is both are acceptable.
   * @return File argument
   */
  def file(name: String, exists: Exists = Exists.Either): Args[JPath] =
    Single(Some(name), PrimType.Path(PathType.File, exists))

  /**
   * Creates a file argument with 'file' as argument name
   *
   * @param exists Yes if path is expected to exists, No otherwise or Either is both are acceptable.
   * @return File argument
   */
  def file(exists: Exists): Args[JPath] =
    Single(None, PrimType.Path(PathType.File, exists))

  /**
   * Creates a file argument with 'file' as argument name, and exists being 'Either'
   */
  val file: Args[JPath] = file(Exists.Either)

  /**
   * Creates a directory argument with a custom argument name
   *
   * @param name Argument name
   * @param exists Yes if path is expected to exists, No otherwise or Either is both are acceptable.
   * @return Directory argument
   */
  def directory(name: String, exists: Exists = Exists.Either): Args[JPath] =
    Single(Some(name), PrimType.Path(PathType.Directory, exists))

  /**
   * Creates a directory argument with 'directory' as argument name
   *
   * @param exists Yes if path is expected to exists, No otherwise or Either is both are acceptable.
   * @return Directory argument
   */
  def directory(exists: Exists): Args[JPath] =
    Single(None, PrimType.Path(PathType.Directory, exists))

  /**
   * Creates a directory argument with 'directory' as argument name, and exists being 'Either'
   */
  val directory: Args[JPath] = directory(Exists.Either)

  /**
   * Creates a text argument with a custom argument name
   *
   * @param name Argument name
   * @return Text argument
   */
  def text(name: String): Args[String] =
    Single(Some(name), PrimType.Text)

  /**
   * Creates a text argument with 'text' as argument name
   */
  val text: Args[String] =
    Single(None, PrimType.Text)

  /**
   * Creates a decimal argument with a custom argument name
   *
   * @param name Argument name
   * @return Decimal argument
   */
  def decimal(name: String): Args[BigDecimal] =
    Single(Some(name), PrimType.Decimal)

  /**
   * Creates a decimal argument with 'decimal' as argument name
   */
  val decimal: Args[BigDecimal] =
    Single(None, PrimType.Decimal)

  /**
   * Creates an integer argument with 'integer' as argument name
   */
  val integer: Args[BigInt] =
    Single(None, PrimType.Integer)

  /**
   * Creates a instant argument with a custom argument name
   *
   * @param name Argument name
   * @return Instant argument
   */
  def instant(name: String): Args[JInstant] =
    Single(Some(name), PrimType.Instant)

  /**
   * Creates a instant argument with 'instant' as argument name
   */
  val instant: Args[JInstant] =
    Single(None, PrimType.Instant)

  /**
   * Creates a localDate argument with a custom argument name
   *
   * @param name Argument name
   * @return LocalDate argument
   */
  def localDate(name: String): Args[JLocalDate] =
    Single(Some(name), PrimType.LocalDate)

  /**
   * Creates a localDateTime argument with a custom argument name
   *
   * @param name Argument name
   * @return LocalDateTime argument
   */
  def localDateTime(name: String): Args[JLocalDateTime] =
    Single(Some(name), PrimType.LocalDateTime)

  /**
   * Creates a localDateTime argument with 'date-time' as argument name
   */
  val localDateTime: Args[JLocalDateTime] =
    Single(None, PrimType.LocalDateTime)

  /**
   * Creates a localTime argument with a custom argument name
   *
   * @param name Argument name
   * @return LocalTime argument
   */
  def localTime(name: String): Args[JLocalTime] =
    Single(Some(name), PrimType.LocalTime)

  /**
   *  Creates a localTime argument with 'local-time' as argument name
   */
  val localTime: Args[JLocalTime] =
    Single(None, PrimType.LocalTime)

  /**
   * Creates a monthDay argument with a custom argument name
   *
   * @param name Argument name
   * @return MonthDay argument
   */
  def monthDay(name: String): Args[JMonthDay] =
    Single(Some(name), PrimType.MonthDay)

  /**
   *  Creates a monthDay argument with 'month-day' as argument name
   */
  val monthDay: Args[JMonthDay] =
    Single(None, PrimType.MonthDay)

  /**
   *  Creates a empty argument
   */
  val none: Args[Unit] = Empty

  /**
   * Creates a offsetDateTime argument with a custom argument name
   *
   * @param name Argument name
   * @return OffsetDateTime argument
   */
  def offsetDateTime(name: String): Args[JOffsetDateTime] =
    Single(Some(name), PrimType.OffsetDateTime)

  /**
   *  Creates a offsetDateTime argument with 'offset-date-time' as argument name
   */
  val offsetDateTime: Args[JOffsetDateTime] =
    Single(None, PrimType.OffsetDateTime)

  /**
   * Creates a offsetTime argument with a custom argument name
   *
   * @param name Argument name
   * @return OffsetTime argument
   */
  def offsetTime(name: String): Args[JOffsetTime] =
    Single(Some(name), PrimType.OffsetTime)

  /**
   *  Creates a offsetTime argument with 'offset-time' as argument name
   */
  val offsetTime: Args[JOffsetTime] =
    Single(None, PrimType.OffsetTime)

  /**
   * Creates a path argument with a custom argument name
   *
   * @param name Argument name
   * @return Path argument
   */
  def path(name: String): Args[JPath] =
    Single(Some(name), PrimType.Path(PathType.Either, Exists.Either))

  /**
   *  Creates a path argument with 'path' as argument name
   */
  val path: Args[JPath] =
    Single(None, PrimType.Path(PathType.Either, Exists.Either))

  /**
   * Creates a period argument with a custom argument name
   *
   * @param name Argument name
   * @return Period argument
   */
  def period(name: String): Args[JPeriod] =
    Single(Some(name), PrimType.Period)

  /**
   *  Creates a period argument with 'period' as argument name
   */
  val period: Args[JPeriod] =
    Single(None, PrimType.Period)

  /**
   * Creates a year argument with a custom argument name
   *
   * @param name Argument name
   * @return Year argument
   */
  def year(name: String): Args[JYear] =
    Single(Some(name), PrimType.Year)

  /**
   *  Creates a year argument with 'year' as argument name
   */
  val year: Args[JYear] =
    Single(None, PrimType.Year)

  /**
   * Creates a yearMonth argument with a custom argument name
   *
   * @param name Argument name
   * @return YearMonth argument
   */
  def yearMonth(name: String): Args[JYearMonth] =
    Single(Some(name), PrimType.YearMonth)

  /**
   *  Creates a yearMonth argument with 'year-month' as argument name
   */
  val yearMonth: Args[JYearMonth] =
    Single(None, PrimType.YearMonth)

  /**
   * Creates a zonedDateTime argument with a custom argument name
   *
   * @param name Argument name
   * @return ZonedDateTime argument
   */
  def zonedDateTime(name: String): Args[JZonedDateTime] =
    Single(Some(name), PrimType.ZonedDateTime)

  /**
   *  Creates a zonedDateTime argument with 'zoned-date-time' as argument name
   */
  val zonedDateTime: Args[JZonedDateTime] =
    Single(None, PrimType.ZonedDateTime)

  /**
   * Creates a zoneId argument with a custom argument name
   *
   * @param name Argument name
   * @return ZoneId argument
   */
  def zoneId(name: String): Args[JZoneId] =
    Single(Some(name), PrimType.ZoneId)

  /**
   *  Creates a zoneId argument with 'zone-id' as argument name
   */
  val zoneId: Args[JZoneId] =
    Single(None, PrimType.ZoneId)

  /**
   * Creates a zoneOffset argument with a custom argument name
   *
   * @param name Argument name
   * @return ZoneOffset argument
   */
  def zoneOffset(name: String): Args[JZoneOffset] =
    Single(Some(name), PrimType.ZoneOffset)

  /**
   *  Creates a zoneOffset argument with 'zone-offset' as argument name
   */
  val zoneOffset: Args[JZoneOffset] =
    Single(None, PrimType.ZoneOffset)
}
