package zio.cli

import zio._
import zio.cli.HelpDoc.Span.text
import zio.cli.files.FileSystem

import java.time.{
  Duration => JDuration,
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
 * A `PrimType` represents the primitive types supported by ZIO CLI.
 *
 * Each primitive type has a way to parse and validate from a string.
 */
sealed trait PrimType[+A] { self =>
  def helpDoc: HelpDoc.Span

  def isBool: Boolean

  def typeName: String

  def choices: Option[String]

  final def validate(value: String): IO[String, A] = validate(Option(value), CliConfig.default)

  final def validate(value: String, conf: CliConfig): IO[String, A] = validate(Option(value), conf)

  def validate(value: Option[String], conf: CliConfig): IO[String, A]
}

object PrimType extends PathPlatformSpecific {

  /**
   * Type representing file system path.
   * @param pathType
   *   Type of expected path: Directory, File or Either if both are acceptable.
   * @param shouldExist
   *   Yes if path is expected to exists, No otherwise or Either is both are acceptable.
   * @param fileSystem
   *   Implementation of FileSystem trait.
   */
  final case class Path(pathType: PathType, shouldExist: Exists, fileSystem: FileSystem = FileSystem.live)
      extends PrimType[JPath] { self =>
    import PathType._

    lazy val helpDoc: HelpDoc.Span = (self.pathType, self.shouldExist) match {
      case (PathType.Either, Exists.Yes)       => text("An existing file or directory.")
      case (PathType.File, Exists.Yes)         => text("An existing file.")
      case (PathType.Directory, Exists.Yes)    => text("An existing directory.")
      case (PathType.Either, Exists.No)        => text("A file or directory that must not exist.")
      case (PathType.File, Exists.No)          => text("A file that must not exist.")
      case (PathType.Directory, Exists.No)     => text("A directory that must not exist.")
      case (PathType.Either, Exists.Either)    => text("A file or directory.")
      case (PathType.File, Exists.Either)      => text("A file.")
      case (PathType.Directory, Exists.Either) => text("A directory.")
    }

    lazy val isBool: Boolean = false

    lazy val typeName: String =
      self.pathType match {
        case PathType.Either    => "path"
        case PathType.File      => "file"
        case PathType.Directory => "directory"
      }

    lazy val choices: Option[String] = None

    def validate(value: Option[String], conf: CliConfig): IO[String, JPath] =
      for {
        value  <- ZIO.fromOption(value).orElseFail("Path options do not have a default value")
        path   <- self.fileSystem.parsePath(value)
        exists <- self.fileSystem.exists(path)
        _      <- validateExistence(value, self.shouldExist, exists)
        _      <- validatePathType(value, path).when(self.shouldExist != Exists.No && exists)
      } yield path

    private def validatePathType(value: String, path: JPath): IO[String, Unit] =
      self.pathType match {
        case Either => ZIO.unit
        case File   =>
          ZIO.fail(s"Expected path '$value' to be a regular file.").unlessZIO(self.fileSystem.isRegularFile(path)).unit
        case Directory =>
          ZIO.fail(s"Expected path '$value' to be a directory.").unlessZIO(self.fileSystem.isDirectory(path)).unit
      }

    private def validateExistence(path: String, expected: Exists, actual: Boolean): IO[String, Unit] =
      (expected, actual) match {
        case (Exists.No, true)   => ZIO.fail(s"Path '$path' must not exist.")
        case (Exists.Yes, false) => ZIO.fail(s"Path '$path' must exist.")
        case _                   => ZIO.unit
      }
  }

  /**
   * Type representing a value selected from set of allowed values.
   * @param cases
   *   lists of allowed parameter-value pairs
   */
  final case class Enumeration[A](cases: (String, A)*) extends PrimType[A] { self =>
    lazy val helpDoc: HelpDoc.Span = text("One of the following cases: " + self.cases.map(_._1).mkString(", ") + ".")

    lazy val isBool: Boolean = false

    lazy val typeName: String = "choice"

    lazy val choices: Option[String] = Some(self.cases.map(_._1).mkString(" | "))

    def validate(value: Option[String], conf: CliConfig): IO[String, A] =
      (ZIO.fromOption(value) orElseFail "Enumeration options do not have a default value.").flatMap { value =>
        self.cases.find(_._1 == value) match {
          case None         => ZIO.fail("Expected one of the following cases: " + self.cases.map(_._1).mkString(", "))
          case Some((_, a)) => ZIO.succeed(a)
        }
      }
  }

  /**
   * Type representing any text.
   */
  case object Text extends PrimType[String] { self =>
    lazy val isBool: Boolean = false

    lazy val typeName: String = "text"

    lazy val choices: Option[String] = None

    def validate(value: Option[String], conf: CliConfig): IO[String, String] =
      attempt(value, v => v, self.typeName)

    lazy val helpDoc: HelpDoc.Span = text("A user-defined piece of text.")
  }

  /**
   * Type representing decimal value via BigDecimal.
   */
  case object Decimal extends PrimType[BigDecimal] { self =>
    lazy val isBool: Boolean = false

    lazy val typeName: String = "decimal"

    lazy val choices: Option[String] = None

    def validate(value: Option[String], conf: CliConfig): IO[String, BigDecimal] =
      attempt(value, BigDecimal(_), self.typeName)

    lazy val helpDoc: HelpDoc.Span = text("A decimal number.")
  }

  /**
   * Type representing integer value via BigInt.
   */
  case object Integer extends PrimType[BigInt] { self =>
    lazy val isBool: Boolean = false

    lazy val typeName: String = "integer"

    lazy val choices: Option[String] = None

    def validate(value: Option[String], conf: CliConfig): IO[String, BigInt] =
      attempt(value, BigInt(_), self.typeName)

    lazy val helpDoc: HelpDoc.Span = text("An integer.")
  }

  /**
   * Type representing a boolean value. True value can be passed as "true", "1", "y", "yes" or "on". False value can be
   * passed as "false", "o", "n", "no" or "off".
   * @param defaultValue
   *   Default value used then param is not provided
   */
  final case class Bool(defaultValue: Option[Boolean]) extends PrimType[Boolean] { self =>
    lazy val isBool: Boolean = true

    lazy val typeName: String = "boolean"

    lazy val choices: Option[String] = Some("true | false")

    def validate(value: Option[String], conf: CliConfig): IO[String, Boolean] =
      value.map(conf.normalizeCase) match {
        case Some(s) if Bool.TrueValues(s)  => ZIO.succeed(true)
        case Some(s) if Bool.FalseValues(s) => ZIO.succeed(false)
        case Some(s)                        => ZIO.fail(s"$s cannot be recognized as valid boolean.")
        case None                           => ZIO.fromOption(self.defaultValue).orElseFail("Missing default for bool parameter.")
      }

    lazy val helpDoc: HelpDoc.Span = text("A true or false value.")
  }

  object Bool {
    lazy val TrueValues: Set[String]  = Set("true", "1", "y", "yes", "on")
    lazy val FalseValues: Set[String] = Set("false", "0", "n", "no", "off")
  }

  /**
   * Type representing a time-based amount of time in the ISO-8601 format, such as 'P1DT2H3M'.
   */
  case object Duration extends PrimType[JDuration] { self =>
    lazy val isBool: Boolean = false

    lazy val typeName: String = "duration"

    lazy val choices: Option[String] = None

    def validate(value: Option[String], conf: CliConfig): IO[String, JDuration] =
      attempt(value, JDuration.parse, self.typeName)

    lazy val helpDoc: HelpDoc.Span =
      text("A time-based amount of time in the ISO-8601 format, such as 'P1DT2H3M'.")
  }

  /**
   * Type representing parameter for instant in time in UTC format, such as 2007-12-03T10:15:30.00Z.
   */
  case object Instant extends PrimType[JInstant] { self =>
    lazy val isBool: Boolean = false

    lazy val typeName: String = "instant"

    lazy val choices: Option[String] = None

    def validate(value: Option[String], conf: CliConfig): IO[String, JInstant] =
      attempt(value, JInstant.parse, self.typeName)

    lazy val helpDoc: HelpDoc.Span = text("An instant in time in UTC format, such as 2007-12-03T10:15:30.00Z.")
  }

  /**
   * Type representing parameter for a date in ISO_LOCAL_DATE format, such as 2007-12-03.
   */
  case object LocalDate extends PrimType[JLocalDate] { self =>
    lazy val isBool: Boolean = false

    lazy val typeName: String = "date"

    lazy val choices: Option[String] = None

    def validate(value: Option[String], conf: CliConfig): IO[String, JLocalDate] =
      attempt(value, JLocalDate.parse, self.typeName)

    lazy val helpDoc: HelpDoc.Span = text("A date in ISO_LOCAL_DATE format, such as 2007-12-03")
  }

  /**
   * Type representing a date-time without a time-zone in the ISO-8601 format, such as 2007-12-03T10:15:30.
   */
  case object LocalDateTime extends PrimType[JLocalDateTime] { self =>
    lazy val isBool: Boolean = false

    lazy val typeName: String = "date-time"

    lazy val choices: Option[String] = None

    def validate(value: Option[String], conf: CliConfig): IO[String, JLocalDateTime] =
      attempt(value, JLocalDateTime.parse, self.typeName)

    lazy val helpDoc: HelpDoc.Span =
      text("A date-time without a time-zone in the ISO-8601 format, such as 2007-12-03T10:15:30.")
  }

  /**
   * Type representing a time without a time-zone in the ISO-8601 format, such as 10:15:30.
   */
  case object LocalTime extends PrimType[JLocalTime] { self =>
    lazy val isBool: Boolean = false

    lazy val typeName: String = "local-time"

    lazy val choices: Option[String] = None

    def validate(value: Option[String], conf: CliConfig): IO[String, JLocalTime] =
      attempt(value, JLocalTime.parse, self.typeName)

    lazy val helpDoc: HelpDoc.Span = text("A time without a time-zone in the ISO-8601 format, such as 10:15:30.")
  }

  /**
   * Type representing a month-day in the ISO-8601 format such as 12-03.
   */
  case object MonthDay extends PrimType[JMonthDay] { self =>
    lazy val isBool: Boolean = false

    lazy val typeName: String = "month-day"

    lazy val choices: Option[String] = None

    def validate(value: Option[String], conf: CliConfig): IO[String, JMonthDay] =
      attempt(value, JMonthDay.parse, self.typeName)

    lazy val helpDoc: HelpDoc.Span = text("A month-day in the ISO-8601 format such as 12-03.")
  }

  /**
   * Type representing a date-time with an offset from UTC/Greenwich in the ISO-8601 format, such as
   * 2007-12-03T10:15:30+01:00.
   */
  case object OffsetDateTime extends PrimType[JOffsetDateTime] { self =>
    lazy val isBool: Boolean = false

    lazy val typeName: String = "offset-date-time"

    lazy val choices: Option[String] = None

    def validate(value: Option[String], conf: CliConfig): IO[String, JOffsetDateTime] =
      attempt(value, JOffsetDateTime.parse, self.typeName)

    lazy val helpDoc: HelpDoc.Span =
      text("A date-time with an offset from UTC/Greenwich in the ISO-8601 format, such as 2007-12-03T10:15:30+01:00.")
  }

  /**
   * Type representing a time with an offset from UTC/Greenwich in the ISO-8601 format, such as 10:15:30+01:00.
   */
  case object OffsetTime extends PrimType[JOffsetTime] { self =>
    lazy val isBool: Boolean = false

    lazy val typeName: String = "offset-time"

    lazy val choices: Option[String] = None

    def validate(value: Option[String], conf: CliConfig): IO[String, JOffsetTime] =
      attempt(value, JOffsetTime.parse, self.typeName)

    lazy val helpDoc: HelpDoc.Span =
      text("A time with an offset from UTC/Greenwich in the ISO-8601 format, such as 10:15:30+01:00.")
  }

  /**
   * Type representing a date-based amount of time in the ISO-8601 format, such as 'P1Y2M3D'.
   */
  case object Period extends PrimType[JPeriod] { self =>
    lazy val isBool: Boolean = false

    lazy val typeName: String = "period"

    lazy val choices: Option[String] = None

    def validate(value: Option[String], conf: CliConfig): IO[String, JPeriod] =
      attempt(value, JPeriod.parse, self.typeName)

    lazy val helpDoc: HelpDoc.Span =
      text("A date-based amount of time in the ISO-8601 format, such as 'P1Y2M3D'.")
  }

  /**
   * Type representing a year in the ISO-8601 format, such as 2007.
   */
  case object Year extends PrimType[JYear] { self =>
    lazy val isBool: Boolean = false

    lazy val typeName: String = "year"

    lazy val choices: Option[String] = None

    def validate(value: Option[String], conf: CliConfig): IO[String, JYear] =
      attempt(value, s => JYear.of(s.toInt), self.typeName)

    lazy val helpDoc: HelpDoc.Span = text("A year in the ISO-8601 format, such as 2007.")
  }

  /**
   * Type representing a year-month in the ISO-8601 format, such as 2007-12..
   */
  case object YearMonth extends PrimType[JYearMonth] { self =>
    lazy val isBool: Boolean = false

    lazy val typeName: String = "year-month"

    lazy val choices: Option[String] = None

    def validate(value: Option[String], conf: CliConfig): IO[String, JYearMonth] = {
      val AcceptedFormat       = "^(-?\\d+)-(\\d{2})".r
      def parse(input: String) = input match {
        case AcceptedFormat(y, m) => ZIO.attempt(JYearMonth.of(y.toInt, m.toInt))
        case _                    => ZIO.fail(())
      }

      (ZIO.fromOption(value) orElseFail "year-month does not have a default value").flatMap(parse) orElseFail
        s"${value.getOrElse("")} is not a ${self.typeName}."
    }

    lazy val helpDoc: HelpDoc.Span = text("A year-month in the ISO-8601 format, such as 2007-12.")
  }

  /**
   * Type representing a date-time with a time-zone in the ISO-8601 format, such as 2007-12-03T10:15:30+01:00
   * Europe/Paris.
   */
  case object ZonedDateTime extends PrimType[JZonedDateTime] { self =>
    lazy val isBool: Boolean = false

    lazy val typeName: String = "zoned-date-time"

    lazy val choices: Option[String] = None

    def validate(value: Option[String], conf: CliConfig): IO[String, JZonedDateTime] =
      attempt(value, JZonedDateTime.parse, self.typeName)

    lazy val helpDoc: HelpDoc.Span =
      text("A date-time with a time-zone in the ISO-8601 format, such as 2007-12-03T10:15:30+01:00 Europe/Paris.")
  }

  /**
   * Type representing a time-zone ID, such as Europe/Paris.
   */
  case object ZoneId extends PrimType[JZoneId] { self =>
    lazy val isBool: Boolean = false

    lazy val typeName: String = "zone-id"

    lazy val choices: Option[String] = None

    def validate(value: Option[String], conf: CliConfig): IO[String, JZoneId] =
      attempt(value, JZoneId.of, self.typeName)

    lazy val helpDoc: HelpDoc.Span = text("A time-zone ID, such as Europe/Paris.")
  }

  /**
   * Type representing a time-zone offset from Greenwich/UTC, such as +02:00.
   */
  case object ZoneOffset extends PrimType[JZoneOffset] { self =>
    lazy val isBool: Boolean = false

    lazy val typeName: String = "zone-offset"

    lazy val choices: Option[String] = None

    def validate(value: Option[String], conf: CliConfig): IO[String, JZoneOffset] =
      attempt(value, JZoneOffset.of, typeName)

    lazy val helpDoc: HelpDoc.Span = text("A time-zone offset from Greenwich/UTC, such as +02:00.")
  }

  private def attempt[A, E](value: Option[String], parse: String => A, typeName: String): IO[String, A] =
    (ZIO.fromOption(value) orElseFail s"$typeName options do not have a default value.").flatMap { value =>
      ZIO.attempt(parse(value)) orElseFail s"$value is not a $typeName."
    }
}
