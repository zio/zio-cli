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
import zio.cli.HelpDoc.p
import zio.cli.HelpDoc.Span._

import scala.collection.immutable.Nil

/**
 * A `Flag[A]` models a command-line flag that produces a value of type `A`.
 */
sealed trait Options[+A] { self =>

  import Options.Single

  final def ++[A1 >: A, That](that: Options[That]): Options[(A1, That)] =
    Options.Cons(self, that)

  final def |[A1 >: A](that: Options[A1]): Options[A1] = self.orElse(that)

  final def orElse[A1 >: A](that: Options[A1]): Options[A1] =
    self.orElseEither(that).map(_.merge)

  final def orElseEither[B](that: Options[B]): Options[Either[A, B]] =
    Options.OrElse(self, that)

  def ??(that: String): Options[A] =
    modifySingle(new SingleModifier {
      override def apply[A](single: Single[A]): Single[A] = single.copy(description = single.description + p(that))
    })

  def alias(name: String): Options[A] =
    modifySingle(new SingleModifier {
      override def apply[A](single: Single[A]): Single[A] = single.copy(aliases = single.aliases :+ name)
    })

  def aliases(names: String*): Options[A] =
    modifySingle(new SingleModifier {
      override def apply[A](single: Single[A]): Single[A] = single.copy(aliases = single.aliases ++ names)
    })

  //TODO : spend time to understand usage of implicit here

  final def as[B, C, Z](f: (B, C) => Z)(implicit ev: A <:< (B, C)): Options[Z] =
    self.map(ev).map { case ((b, c)) => f(b, c) }

  final def as[B, C, D, Z](f: (B, C, D) => Z)(implicit ev: A <:< ((B, C), D)): Options[Z] =
    self.map(ev).map { case ((b, c), d) => f(b, c, d) }

  final def as[B, C, D, E, Z](f: (B, C, D, E) => Z)(implicit ev: A <:< (((B, C), D), E)): Options[Z] =
    self.map(ev).map { case (((b, c), d), e) => f(b, c, d, e) }

  final def as[B, C, D, E, F, Z](f0: (B, C, D, E, F) => Z)(implicit ev: A <:< ((((B, C), D), E), F)): Options[Z] =
    self.map(ev).map { case ((((b, c), d), e), f) => f0(b, c, d, e, f) }

  final def as[B, C, D, E, F, G, Z](
    f0: (B, C, D, E, F, G) => Z
  )(implicit ev: A <:< (((((B, C), D), E), F), G)): Options[Z] =
    self.map(ev).map { case (((((b, c), d), e), f), g) => f0(b, c, d, e, f, g) }

  final def fold[B, C, Z](
    f1: B => Z,
    f2: C => Z
  )(implicit ev: A <:< Either[B, C]): Options[Z] =
    self.map(ev).map {
      case Left(b)  => f1(b)
      case Right(c) => f2(c)
    }

  final def fold[B, C, D, Z](
    f1: B => Z,
    f2: C => Z,
    f3: D => Z
  )(implicit ev: A <:< Either[Either[B, C], D]): Options[Z] =
    self.map(ev).map {
      case Left(Left(b))  => f1(b)
      case Left(Right(c)) => f2(c)
      case Right(d)       => f3(d)
    }

  final def fold[B, C, D, E, Z](
    f1: B => Z,
    f2: C => Z,
    f3: D => Z,
    f4: E => Z
  )(implicit ev: A <:< Either[Either[Either[B, C], D], E]): Options[Z] =
    self.map(ev).map {
      case Left(Left(Left(b)))  => f1(b)
      case Left(Left(Right(c))) => f2(c)
      case Left(Right(d))       => f3(d)
      case Right(e)             => f4(e)
    }

  final def fold[B, C, D, E, F, Z](
    f1: B => Z,
    f2: C => Z,
    f3: D => Z,
    f4: E => Z,
    f5: F => Z
  )(implicit ev: A <:< Either[Either[Either[Either[B, C], D], E], F]): Options[Z] =
    self.map(ev).map {
      case Left(Left(Left(Left(b))))  => f1(b)
      case Left(Left(Left(Right(c)))) => f2(c)
      case Left(Left(Right(d)))       => f3(d)
      case Left(Right(e))             => f4(e)
      case Right(f)                   => f5(f)
    }

  final def fold[B, C, D, E, F, G, Z](
    f1: B => Z,
    f2: C => Z,
    f3: D => Z,
    f4: E => Z,
    f5: F => Z,
    f6: G => Z
  )(implicit ev: A <:< Either[Either[Either[Either[Either[B, C], D], E], F], G]): Options[Z] =
    self.map(ev).map {
      case Left(Left(Left(Left(Left(b)))))  => f1(b)
      case Left(Left(Left(Left(Right(c))))) => f2(c)
      case Left(Left(Left(Right(d))))       => f3(d)
      case Left(Left(Right(e)))             => f4(e)
      case Left(Right(f))                   => f5(f)
      case Right(g)                         => f6(g)
    }

  final def collect[B](message: String)(f: PartialFunction[A, B]): Options[B] =
    Options.Map(
      self,
      (a: A) =>
        f.lift(a)
          .fold[Either[ValidationError, B]](Left(ValidationError(ValidationErrorType.InvalidValue, p(error(message)))))(
            Right(_)
          )
    )

  final def flatten2[B, C](implicit ev: A <:< ((B, C))): Options[(B, C)] = as[B, C, (B, C)]((_, _))

  final def flatten3[B, C, D](implicit ev: A <:< ((B, C), D)): Options[(B, C, D)] = as[B, C, D, (B, C, D)]((_, _, _))

  final def flatten4[B, C, D, E](implicit ev: A <:< (((B, C), D), E)): Options[(B, C, D, E)] =
    as[B, C, D, E, (B, C, D, E)]((_, _, _, _))

  final def flatten5[B, C, D, E, F](implicit ev: A <:< ((((B, C), D), E), F)): Options[(B, C, D, E, F)] =
    as[B, C, D, E, F, (B, C, D, E, F)]((_, _, _, _, _))

  final def flatten6[B, C, D, E, F, G](implicit ev: A <:< (((((B, C), D), E), F), G)): Options[(B, C, D, E, F, G)] =
    as[B, C, D, E, F, G, (B, C, D, E, F, G)]((_, _, _, _, _, _))

  def helpDoc: HelpDoc

  final def map[B](f: A => B): Options[B] = Options.Map(self, (a: A) => Right(f(a)))

  final def mapOrFail[B](f: A => Either[ValidationError, B]): Options[B] =
    Options.Map(self, (a: A) => f(a))

  final def mapTry[B](f: A => B): Options[B] =
    self.mapOrFail((a: A) =>
      scala.util.Try(f(a)).toEither.left.map(e => ValidationError(ValidationErrorType.InvalidValue, p(e.getMessage)))
    )

  final def optional(desc: String): Options[Option[A]] = self.map(Some(_)).withDefault(None, desc)

  def synopsis: UsageSynopsis

  def uid: Option[String]

  def validate(args: List[String], conf: CliConfig): IO[ValidationError, (List[String], A)]

  def withDefault[A1 >: A](value: A1, valueDescription: String): Options[A1] =
    Options.WithDefault(self, value, valueDescription)

  private[cli] def modifySingle(f: SingleModifier): Options[A]
}

trait SingleModifier {
  def apply[A](single: Options.Single[A]): Options.Single[A]
}

object Options {
  case object Empty extends Options[Unit] {
    def synopsis: UsageSynopsis = UsageSynopsis.None

    def validate(args: List[String], conf: CliConfig): IO[ValidationError, (List[String], Unit)] =
      IO.succeed((args, ()))

    override def modifySingle(f: SingleModifier): Options[Unit] = Empty

    override def helpDoc: HelpDoc = HelpDoc.Empty

    override def uid: Option[String] = None
  }

  final case class WithDefault[A](options: Options[A], default: A, defaultDescription: String) extends Options[A] {
    def synopsis: UsageSynopsis = options.synopsis

    def validate(args: List[String], conf: CliConfig): IO[ValidationError, (List[String], A)] =
      options
        .validate(args, conf)
        .foldM(
          invalid =>
            if (invalid.isOptionMissing) {
              IO.succeed(args -> default)
            } else {
              IO.fail(invalid)
            },
          success => IO.succeed(success)
        )

    override def modifySingle(f: SingleModifier): Options[A] =
      WithDefault(options.modifySingle(f), default, defaultDescription)

    override def helpDoc: HelpDoc =
      options.helpDoc.mapDescriptionList {
        case (span, block) =>
          span -> (block + HelpDoc.p(
            s"This setting is optional. If unspecified, the default value of this option is ${default}. ${defaultDescription}"
          ))
      }

    override def uid: Option[String] = options.uid
  }

  final case class Single[+A](
    name: String,
    aliases: Vector[String],
    primType: PrimType[A],
    description: HelpDoc = HelpDoc.Empty
  ) extends Options[A] { self =>

    override def modifySingle(f: SingleModifier): Options[A] = f(self)

    def synopsis: UsageSynopsis =
      UsageSynopsis.Named(fullname, primType.choices)

    private def supports(s: String, names: List[String], conf: CliConfig): Boolean =
      if (conf.caseSensitive)
        names.contains(s)
      else
        names.exists(_.equalsIgnoreCase(s))

    def validate(args: List[String], conf: CliConfig): IO[ValidationError, (List[String], A)] = {
      val names = fullname :: aliases.map(makeFullName).toList
      args match {
        case head :: tail if supports(head, names, conf) =>
          primType match {
            case _: PrimType.Bool =>
              primType
                .validate(None, conf)
                .bimap(f => ValidationError(ValidationErrorType.InvalidValue, p(f)), tail -> _)
            case _ =>
              (tail match {
                case Nil         => primType.validate(None, conf)
                case ::(head, _) => primType.validate(Some(head), conf)
              }).bimap(f => ValidationError(ValidationErrorType.InvalidValue, p(f)), a => tail.drop(1) -> a)
          }
        case head :: tail
            if name.length > conf.autoCorrectLimit + 1 && AutoCorrect.levensteinDistance(head, fullname, conf) <= conf.autoCorrectLimit =>
          IO.fail(
            ValidationError(
              ValidationErrorType.MissingValue,
              p(error(s"""The flag "${head}" is not recognized. Did you mean ${fullname}?"""))
            )
          )
        case head :: tail =>
          validate(tail, conf).map {
            case (args, a) => (head :: args, a)
          }
        case Nil =>
          IO.fail(ValidationError(ValidationErrorType.MissingValue, p(error(s"Expected to find ${fullname} option."))))
      }
    }

    def uid = Some(fullname)

    private def makeFullName(s: String): String = (if (s.length == 1) "-" else "--") + s

    private def fullname: String = makeFullName(name)

    override def helpDoc: HelpDoc = {

      val allNames = Vector("--" + name) ++ aliases.map("--" + _)

      HelpDoc.DescriptionList(
        List(
          spans(allNames.map(weak(_)).zipWithIndex.map {
            case (span, index) => if (index != allNames.length - 1) span + Span.text(", ") else span
          }) ->
            (p(primType.helpDoc) + description)
        )
      )
    }
  }

  final case class OrElse[A, B](left: Options[A], right: Options[B]) extends Options[Either[A, B]] {
    override def modifySingle(f: SingleModifier): Options[Either[A, B]] =
      OrElse(left.modifySingle(f), right.modifySingle(f))

    def synopsis: UsageSynopsis = left.synopsis + right.synopsis

    override def validate(args: List[String], conf: CliConfig): IO[ValidationError, (List[String], Either[A, B])] =
      left
        .validate(args, conf)
        .foldM(
          err1 =>
            right
              .validate(args, conf)
              .foldM[Any, ValidationError, (List[String], Either[A, B])](
                err2 =>
                  IO.fail(
                    // orElse option is only missing in case neither option was given
                    (err1.validationErrorType, err2.validationErrorType) match {
                      case (ValidationErrorType.MissingValue, ValidationErrorType.MissingValue) =>
                        ValidationError(ValidationErrorType.MissingValue, err1.error + err2.error)
                      case _ =>
                        ValidationError(ValidationErrorType.InvalidValue, err1.error + err2.error)
                    }
                  ),
                success => IO.succeed((success._1, Right(success._2)))
              ),
          r =>
            right
              .validate(r._1, conf)
              .foldM(
                _ => IO.succeed((r._1, Left(r._2))),
                _ =>
                  IO.fail(
                    ValidationError(
                      ValidationErrorType.InvalidValue,
                      p(error(s"Options collision detected. You can only specify either ${left} or ${right}."))
                    )
                  )
              )
        )

    override def helpDoc: HelpDoc = left.helpDoc + right.helpDoc

    override def uid: Option[String] = (left.uid.toList ++ right.uid.toList) match {
      case Nil  => None
      case list => Some(list.mkString(", "))
    }
  }

  final case class Cons[A, B](left: Options[A], right: Options[B]) extends Options[(A, B)] {
    override def modifySingle(f: SingleModifier): Options[(A, B)] = Cons(left.modifySingle(f), right.modifySingle(f))

    def synopsis: UsageSynopsis = left.synopsis + right.synopsis

    override def validate(args: List[String], conf: CliConfig): IO[ValidationError, (List[String], (A, B))] =
      for {
        tuple <- left
                  .validate(args, conf)
                  .catchAll(err1 =>
                    right
                      .validate(args, conf)
                      .foldM(
                        err2 => IO.fail(ValidationError(ValidationErrorType.MissingValue, err1.error + err2.error)),
                        _ => IO.fail(err1)
                      )
                  )
        (args, a) = tuple
        tuple     <- right.validate(args, conf)
        (args, b) = tuple
      } yield (args -> (a -> b))

    override def helpDoc: HelpDoc = left.helpDoc + right.helpDoc

    override def uid: Option[String] = (left.uid.toList ++ right.uid.toList) match {
      case Nil  => None
      case list => Some(list.mkString(", "))
    }
  }

  final case class Map[A, B](value: Options[A], f: A => Either[ValidationError, B]) extends Options[B] {
    override def modifySingle(f0: SingleModifier): Options[B] = Map(value.modifySingle(f0), f)

    def synopsis: UsageSynopsis = value.synopsis

    def validate(args: List[String], conf: CliConfig): IO[ValidationError, (List[String], B)] =
      value.validate(args, conf).flatMap(r => f(r._2).fold(e => IO.fail(e), s => IO.succeed(r._1 -> s)))

    override def uid: Option[String] = value.uid

    override def helpDoc: HelpDoc = value.helpDoc
  }

  /**
   * Creates a boolean flag with the specified name, which, if present, will
   * produce the specified constant boolean value.
   */
  def bool(name: String, ifPresent: Boolean): Options[Boolean] = makeBool(name, ifPresent, Nil)

  /**
   * Creates a boolean flag with the specified name, which, if present, will
   * produce the specified constant boolean value.
   * Negation names may be specified to explicitly invert the boolean value of this option.
   */
  def bool(name: String, ifPresent: Boolean, negationName: String, negationNames: String*): Options[Boolean] =
    makeBool(name, ifPresent, negationName :: negationNames.toList)

  private def makeBool(name: String, ifPresent: Boolean, negationNames: List[String]): Options[Boolean] = {

    val option = Single(name, Vector.empty, PrimType.Bool(Some(ifPresent)))

    negationNames match {
      case Nil =>
        option.withDefault(!ifPresent, (!ifPresent).toString)
      case head :: tail =>
        val negationOption = Single(head, tail.toVector, PrimType.Bool(Some(!ifPresent)))
        (option | negationOption).withDefault(!ifPresent, (!ifPresent).toString)
    }
  }

  /**
   * Creates a parameter accepting one valye from set of allowed elements.
   */
  def enumeration[A](name: String)(cases: (String, A)*): Options[A] =
    Single(name, Vector.empty, PrimType.Enumeration(cases: _*))

  /**
   * Creates a parameter expecting path to the file.
   */
  def file(name: String, exists: Exists = Exists.Either): Options[JPath] =
    Single(name, Vector.empty, PrimType.Path(PathType.File, exists))

  /**
   * Creates a parameter expecting path to the directory.
   * */
  def directory(name: String, exists: Exists = Exists.Either): Options[JPath] =
    Single(name, Vector.empty, PrimType.Path(PathType.Directory, exists))

  /**
   * Creates a parameter expecting an arbitrary text.
   */
  def text(name: String): Options[String] =
    Single(name, Vector.empty, PrimType.Text)

  /**
   * Creates a parameter expecting a decimal number.
   */
  def decimal(name: String): Options[BigDecimal] =
    Single(name, Vector.empty, PrimType.Decimal)

  /**
   * Creates a parameter expecting an integer.
   */
  def integer(name: String): Options[BigInt] =
    Single(name, Vector.empty, PrimType.Integer)

  /**
   * Creates a parameter expecting a parameter for instant in time in UTC format, such as 2007-12-03T10:15:30.00Z.
   */
  def instant(name: String): Options[JInstant] =
    Single(name, Vector.empty, PrimType.Instant)

  /**
   * Creates a parameter excepting parameter for a date in ISO_LOCAL_DATE format, such as 2007-12-03.
   */
  def localDate(name: String): Options[JLocalDate] =
    Single(name, Vector.empty, PrimType.LocalDate)

  /**
   * Creates a parameter excepting a date-time without a time-zone in the ISO-8601 format, such as 2007-12-03T10:15:30.
   */
  def localDateTime(name: String): Options[JLocalDateTime] =
    Single(name, Vector.empty, PrimType.LocalDateTime)

  /**
   * Creates a parameter excepting a time without a time-zone in the ISO-8601 format, such as 10:15:30.
   */
  def localTime(name: String): Options[JLocalTime] =
    Single(name, Vector.empty, PrimType.LocalTime)

  /**
   * Creates a parameter excepting a month-day in the ISO-8601 format such as 12-03.
   */
  def monthDay(name: String): Options[JMonthDay] =
    Single(name, Vector.empty, PrimType.MonthDay)

  val none: Options[Unit] = Empty

  /**
   * Creates a parameter excepting a date-time with an offset from UTC/Greenwich in the ISO-8601 format, such as 2007-12-03T10:15:30+01:00.
   */
  def offsetDateTime(name: String): Options[JOffsetDateTime] =
    Single(name, Vector.empty, PrimType.OffsetDateTime)

  /**
   * Creates a parameter excepting a time with an offset from UTC/Greenwich in the ISO-8601 format, such as 10:15:30+01:00.
   */
  def offsetTime(name: String): Options[JOffsetTime] =
    Single(name, Vector.empty, PrimType.OffsetTime)

  /**
   * Createsa parameter excepting  a date-based amount of time in the ISO-8601 format, such as 'P1Y2M3D'.
   */
  def period(name: String): Options[JPeriod] =
    Single(name, Vector.empty, PrimType.Period)

  /**
   * Creates a parameter expecting a year in the ISO-8601 format, such as 2007.
   */
  def year(name: String): Options[JYear] =
    Single(name, Vector.empty, PrimType.Year)

  /**
   * Creates a parameter expecting a year-month in the ISO-8601 format, such as 2007-12..
   */
  def yearMonth(name: String): Options[JYearMonth] =
    Single(name, Vector.empty, PrimType.YearMonth)

  /**
   * Creates a date-time with a time-zone in the ISO-8601 format, such as 2007-12-03T10:15:30+01:00 Europe/Paris.
   */
  def zonedDateTime(name: String): Options[JZonedDateTime] =
    Single(name, Vector.empty, PrimType.ZonedDateTime)

  /**
   * Creates a parameter expecting a time-zone ID, such as Europe/Paris.
   */
  def zoneId(name: String): Options[JZoneId] =
    Single(name, Vector.empty, PrimType.ZoneId)

  /**
   * Creates a parameter expecting a time-zone offset from Greenwich/UTC, such as +02:00.
   */
  def zoneOffset(name: String): Options[JZoneOffset] =
    Single(name, Vector.empty, PrimType.ZoneOffset)

}
