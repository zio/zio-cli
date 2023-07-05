package zio.cli

import zio._
import zio.cli.HelpDoc.{Span, p}

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

/**
 * A `Args` represents arguments that can be passed to a command-line application.
 */
sealed trait Args[+A] extends Parameter { self =>

  final def ++[B](that: Args[B])(implicit zippable: Zippable[A, B]): Args[zippable.Out] =
    Args.Both(self, that).map { case (a, b) => zippable.zip(a, b) }

  final def +[A1 >: A]: Args[::[A1]] = Args.Variadic(self, Some(1), None).map {
    case head :: tail => ::(head, tail)
    case Nil          => throw new IllegalStateException("Args.Variadic is not respecting the minimum.")
  }

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

  final def repeat1[A1 >: A]: Args[::[A1]] = self.+

  def synopsis: UsageSynopsis

  def uid: Option[String]

  def validate(args: List[String], conf: CliConfig): IO[ValidationError, (List[String], A)]

  lazy val tag = "argument"

}

object Args {

  final case class Single[+A](pseudoName: Option[String], primType: PrimType[A], description: HelpDoc = HelpDoc.Empty)
      extends Args[A]
      with Input {
    self =>

    override lazy val shortDesc: String = s"Argument $name: ${description.getSpan.text}"

    def ??(that: String): Args[A] = copy(description = self.description + HelpDoc.p(that))

    lazy val helpDoc: HelpDoc =
      HelpDoc.DescriptionList(
        List(
          Span.weak(name) -> (p(self.primType.helpDoc) + self.description)
        )
      )

    lazy val maxSize: Int = 1

    lazy val minSize: Int = 1

    lazy val synopsis: UsageSynopsis = UsageSynopsis.Named(List(name), self.primType.choices)

    def validate(args: List[String], conf: CliConfig): IO[ValidationError, (List[String], A)] =
      (args match {
        case head :: tail => self.primType.validate(Some(head), conf).mapBoth(text => HelpDoc.p(text), a => tail -> a)
        case Nil =>
          val msg = (self.pseudoName, self.primType.choices) match {
            case (Some(pseudoName), Some(choices)) => s"Missing argument <$pseudoName> with values $choices."
            case (Some(pseudoName), _)             => s"Missing argument <$pseudoName>."
            case (None, Some(choices))             => s"Missing argument ${self.primType.typeName} with values $choices."
            case (None, None)                      => s"Missing argument ${self.primType.typeName}."
          }
          ZIO.fail(HelpDoc.p(msg))
      }).mapError(ValidationError(ValidationErrorType.InvalidArgument, _))

    private def name: String = "<" + self.pseudoName.getOrElse(self.primType.typeName) + ">"

    def uid: Option[String] = Some(self.name)

    override def isValid(input: String, conf: CliConfig): IO[ValidationError, List[String]] =
      for {
        _ <- validate(List(input), conf)
      } yield List(input)
  }

  case object Empty extends Args[Unit] with Pipeline {
    def ??(that: String): Args[Unit] = Empty

    lazy val helpDoc: HelpDoc = HelpDoc.Empty

    lazy val maxSize: Int = 0

    lazy val minSize: Int = 0

    lazy val synopsis: UsageSynopsis = UsageSynopsis.None

    def validate(args: List[String], conf: CliConfig): UIO[(List[String], Unit)] =
      ZIO.succeed((args, ()))

    def uid: Option[String] = None

    override def pipeline = ("", List())
  }

  final case class Both[+A, +B](head: Args[A], tail: Args[B]) extends Args[(A, B)] with Pipeline { self =>
    def ??(that: String): Args[(A, B)] = Both(self.head ?? that, self.tail ?? that)

    lazy val helpDoc: HelpDoc = self.head.helpDoc + self.tail.helpDoc

    lazy val maxSize: Int = self.head.maxSize + self.tail.maxSize

    lazy val minSize: Int = self.head.minSize + self.tail.minSize

    lazy val synopsis: UsageSynopsis = self.head.synopsis + self.tail.synopsis

    def validate(args: List[String], conf: CliConfig): IO[ValidationError, (List[String], (A, B))] =
      for {
        tuple    <- self.head.validate(args, conf)
        (args, a) = tuple
        tuple    <- self.tail.validate(args, conf)
        (args, b) = tuple
      } yield (args, (a, b))

    def uid: Option[String] = self.head.uid.toList ++ self.tail.uid.toList match {
      case Nil  => None
      case list => Some(list.mkString(", "))
    }

    override def pipeline = ("", List(head, tail))
  }

  final case class Variadic[+A](value: Args[A], min: Option[Int], max: Option[Int]) extends Args[List[A]] with Input {
    self =>

    override lazy val shortDesc: String = helpDoc.toPlaintext()

    def ??(that: String): Args[List[A]] = Variadic(self.value ?? that, self.min, self.max)

    lazy val synopsis: UsageSynopsis = UsageSynopsis.Repeated(self.value.synopsis)

    lazy val helpDoc: HelpDoc = self.value.helpDoc.mapDescriptionList { case (span, block) =>
      val newSpan = span + Span.text(
        if (self.max.isDefined) s" $minSize - $maxSize" else if (minSize == 0) "..." else s" $minSize+"
      )
      val newBlock =
        block +
          HelpDoc.p(
            if (self.max.isDefined)
              s"This argument must be repeated at least $minSize times and may be repeated up to $maxSize times."
            else if (minSize == 0) "This argument may be repeated zero or more times."
            else s"This argument must be repeated at least $minSize times."
          )

      (newSpan, newBlock)
    }

    lazy val maxSize: Int = self.max.getOrElse(Int.MaxValue / 2) * self.value.maxSize

    lazy val minSize: Int = self.min.getOrElse(0) * value.minSize

    def validate(args: List[String], conf: CliConfig): IO[ValidationError, (List[String], List[A])] = {
      val min1 = min.getOrElse(0)
      val max1 = self.max.getOrElse(Int.MaxValue)

      def loop(args: List[String], acc: List[A]): IO[ValidationError, (List[String], List[A])] =
        if (acc.length >= max1) ZIO.succeed(args -> acc)
        else
          self.value
            .validate(args, conf)
            .foldZIO(
              failure => if (acc.length >= min1 && args.isEmpty) ZIO.succeed(args -> acc) else ZIO.fail(failure),
              { case (args, a) => loop(args, a :: acc) }
            )

      loop(args, Nil).map { case (args, list) => (args, list.reverse) }
    }

    def uid: Option[String] = self.value.uid

    override def isValid(input: String, conf: CliConfig): IO[ValidationError, List[String]] =
      for {
        list <- ZIO.succeed(input.split(" ").toList)
        _    <- validate(list, conf)
      } yield list

  }

  final case class Map[A, B](value: Args[A], f: A => Either[HelpDoc, B]) extends Args[B] with Pipeline with Wrap {
    self =>
    override lazy val shortDesc: String = value.shortDesc

    def ??(that: String): Args[B] = Map(self.value ?? that, self.f)

    lazy val helpDoc: HelpDoc = self.value.helpDoc

    lazy val maxSize: Int = self.value.maxSize

    lazy val minSize: Int = self.value.minSize

    lazy val synopsis: UsageSynopsis = self.value.synopsis

    def validate(args: List[String], conf: CliConfig): IO[ValidationError, (List[String], B)] =
      self.value.validate(args, conf).flatMap { case (r, a) =>
        self.f(a) match {
          case Left(value)  => ZIO.fail(ValidationError(ValidationErrorType.InvalidArgument, value))
          case Right(value) => ZIO.succeed((r, value))
        }
      }

    def uid: Option[String] = self.value.uid

    override val wrapped = value

    override val pipeline = ("", List(value))
  }

  /**
   * Creates a boolean argument with a custom argument name
   *
   * @param name
   *   Argument name
   * @return
   *   Boolean argument
   */
  def bool(name: String): Args[Boolean] = Single(Some(name), PrimType.Bool(None))

  /**
   * Creates a boolean argument with boolean as argument name
   */
  val bool: Args[Boolean] = Single(None, PrimType.Bool(None))

  /**
   * Creates a enumeration argument with a custom argument name
   *
   * @param name
   *   Argument name
   * @param cases
   *   Enum cases
   * @return
   *   Enumeration argument
   */
  def enumeration[A](name: String)(cases: (String, A)*): Args[A] =
    Single(Some(name), PrimType.Enumeration(cases: _*))

  /**
   * Creates a enumeration argument with 'choice' as argument name
   *
   * @param cases
   *   Enum cases
   * @return
   *   Enumeration argument
   */
  def enumeration[A](cases: (String, A)*): Args[A] =
    Single(None, PrimType.Enumeration(cases: _*))

  /**
   * Creates a file argument with a custom argument name
   *
   * @param name
   *   Argument name
   * @param exists
   *   Yes if path is expected to exists, No otherwise or Either is both are acceptable.
   * @return
   *   File argument
   */
  def file(name: String, exists: Exists = Exists.Either): Args[JPath] =
    Single(Some(name), PrimType.Path(PathType.File, exists))

  /**
   * Creates a file argument with 'file' as argument name
   *
   * @param exists
   *   Yes if path is expected to exists, No otherwise or Either is both are acceptable.
   * @return
   *   File argument
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
   * @param name
   *   Argument name
   * @param exists
   *   Yes if path is expected to exists, No otherwise or Either is both are acceptable.
   * @return
   *   Directory argument
   */
  def directory(name: String, exists: Exists = Exists.Either): Args[JPath] =
    Single(Some(name), PrimType.Path(PathType.Directory, exists))

  /**
   * Creates a directory argument with 'directory' as argument name
   *
   * @param exists
   *   Yes if path is expected to exists, No otherwise or Either is both are acceptable.
   * @return
   *   Directory argument
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
   * @param name
   *   Argument name
   * @return
   *   Text argument
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
   * @param name
   *   Argument name
   * @return
   *   Decimal argument
   */
  def decimal(name: String): Args[BigDecimal] =
    Single(Some(name), PrimType.Decimal)

  /**
   * Creates a decimal argument with 'decimal' as argument name
   */
  val decimal: Args[BigDecimal] =
    Single(None, PrimType.Decimal)

  /**
   * Creates an integer argument with a custom argument name
   *
   * @param name
   *   Argument name
   * @return
   *   Integer argument
   */
  def integer(name: String): Args[BigInt] =
    Single(Some(name), PrimType.Integer)

  /**
   * Creates an integer argument with 'integer' as argument name
   */
  val integer: Args[BigInt] =
    Single(None, PrimType.Integer)

  /**
   * Creates a instant argument with a custom argument name
   *
   * @param name
   *   Argument name
   * @return
   *   Instant argument
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
   * @param name
   *   Argument name
   * @return
   *   LocalDate argument
   */
  def localDate(name: String): Args[JLocalDate] =
    Single(Some(name), PrimType.LocalDate)

  /**
   * Creates a localDateTime argument with a custom argument name
   *
   * @param name
   *   Argument name
   * @return
   *   LocalDateTime argument
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
   * @param name
   *   Argument name
   * @return
   *   LocalTime argument
   */
  def localTime(name: String): Args[JLocalTime] =
    Single(Some(name), PrimType.LocalTime)

  /**
   * Creates a localTime argument with 'local-time' as argument name
   */
  val localTime: Args[JLocalTime] =
    Single(None, PrimType.LocalTime)

  /**
   * Creates a monthDay argument with a custom argument name
   *
   * @param name
   *   Argument name
   * @return
   *   MonthDay argument
   */
  def monthDay(name: String): Args[JMonthDay] =
    Single(Some(name), PrimType.MonthDay)

  /**
   * Creates a monthDay argument with 'month-day' as argument name
   */
  val monthDay: Args[JMonthDay] =
    Single(None, PrimType.MonthDay)

  /**
   * Creates a empty argument
   */
  val none: Args[Unit] = Empty

  /**
   * Creates a offsetDateTime argument with a custom argument name
   *
   * @param name
   *   Argument name
   * @return
   *   OffsetDateTime argument
   */
  def offsetDateTime(name: String): Args[JOffsetDateTime] =
    Single(Some(name), PrimType.OffsetDateTime)

  /**
   * Creates a offsetDateTime argument with 'offset-date-time' as argument name
   */
  val offsetDateTime: Args[JOffsetDateTime] =
    Single(None, PrimType.OffsetDateTime)

  /**
   * Creates a offsetTime argument with a custom argument name
   *
   * @param name
   *   Argument name
   * @return
   *   OffsetTime argument
   */
  def offsetTime(name: String): Args[JOffsetTime] =
    Single(Some(name), PrimType.OffsetTime)

  /**
   * Creates a offsetTime argument with 'offset-time' as argument name
   */
  val offsetTime: Args[JOffsetTime] =
    Single(None, PrimType.OffsetTime)

  /**
   * Creates a path argument with a custom argument name
   *
   * @param name
   *   Argument name
   * @return
   *   Path argument
   */
  def path(name: String): Args[JPath] =
    Single(Some(name), PrimType.Path(PathType.Either, Exists.Either))

  /**
   * Creates a path argument with 'path' as argument name
   */
  val path: Args[JPath] =
    Single(None, PrimType.Path(PathType.Either, Exists.Either))

  /**
   * Creates a period argument with a custom argument name
   *
   * @param name
   *   Argument name
   * @return
   *   Period argument
   */
  def period(name: String): Args[JPeriod] =
    Single(Some(name), PrimType.Period)

  /**
   * Creates a period argument with 'period' as argument name
   */
  val period: Args[JPeriod] =
    Single(None, PrimType.Period)

  /**
   * Creates a year argument with a custom argument name
   *
   * @param name
   *   Argument name
   * @return
   *   Year argument
   */
  def year(name: String): Args[JYear] =
    Single(Some(name), PrimType.Year)

  /**
   * Creates a year argument with 'year' as argument name
   */
  val year: Args[JYear] =
    Single(None, PrimType.Year)

  /**
   * Creates a yearMonth argument with a custom argument name
   *
   * @param name
   *   Argument name
   * @return
   *   YearMonth argument
   */
  def yearMonth(name: String): Args[JYearMonth] =
    Single(Some(name), PrimType.YearMonth)

  /**
   * Creates a yearMonth argument with 'year-month' as argument name
   */
  val yearMonth: Args[JYearMonth] =
    Single(None, PrimType.YearMonth)

  /**
   * Creates a zonedDateTime argument with a custom argument name
   *
   * @param name
   *   Argument name
   * @return
   *   ZonedDateTime argument
   */
  def zonedDateTime(name: String): Args[JZonedDateTime] =
    Single(Some(name), PrimType.ZonedDateTime)

  /**
   * Creates a zonedDateTime argument with 'zoned-date-time' as argument name
   */
  val zonedDateTime: Args[JZonedDateTime] =
    Single(None, PrimType.ZonedDateTime)

  /**
   * Creates a zoneId argument with a custom argument name
   *
   * @param name
   *   Argument name
   * @return
   *   ZoneId argument
   */
  def zoneId(name: String): Args[JZoneId] =
    Single(Some(name), PrimType.ZoneId)

  /**
   * Creates a zoneId argument with 'zone-id' as argument name
   */
  val zoneId: Args[JZoneId] =
    Single(None, PrimType.ZoneId)

  /**
   * Creates a zoneOffset argument with a custom argument name
   *
   * @param name
   *   Argument name
   * @return
   *   ZoneOffset argument
   */
  def zoneOffset(name: String): Args[JZoneOffset] =
    Single(Some(name), PrimType.ZoneOffset)

  /**
   * Creates a zoneOffset argument with 'zone-offset' as argument name
   */
  val zoneOffset: Args[JZoneOffset] =
    Single(None, PrimType.ZoneOffset)
}
