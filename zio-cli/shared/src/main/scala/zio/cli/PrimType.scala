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

import zio._
import zio.cli.HelpDoc.Span.text
import zio.cli.files.FileSystem

/**
 * A `PrimType` represents the primitive types supported by ZIO CLI.
 *
 * Each primitive type has a way to parse and validate from a string.
 */
sealed trait PrimType[+A] {
  def helpDoc: HelpDoc.Span

  def typeName: String

  def choices: Option[String]

  final def validate(value: String): IO[String, A] = validate(Option(value), CLIConfig.default)

  final def validate(value: String, conf: CLIConfig): IO[String, A] = validate(Option(value), conf)

  def validate(value: Option[String], conf: CLIConfig): IO[String, A]
}

object PrimType {

  /**
   * Type representing file system path.
   * @param pathType Type of expected path: Directory, File or Either if both are acceptable.
   * @param exists Yes if path is expected to exists, No otherwise or Either is both are acceptable.
   * @param fileSystem Implementation of FileSystem trait.
   */
  final case class Path(pathType: PathType, exists: Exists, fileSystem: FileSystem = FileSystem.live)
      extends PrimType[JPath] {
    import PathType._

    def helpDoc: HelpDoc.Span = (pathType, exists) match {
      case (PathType.Either, Exists.Yes)       => text("An existing file or directory.")
      case (PathType.File, Exists.Yes)         => text("An existing file.")
      case (PathType.Directory, Exists.Yes)    => text("An existing directory.")
      case (PathType.Either, Exists.No)        => text("A file or directory that must not exist.")
      case (PathType.File, Exists.No)          => text("A file that does not exist.")
      case (PathType.Directory, Exists.No)     => text("A directory that does not exist.")
      case (PathType.Either, Exists.Either)    => text("A file or directory.")
      case (PathType.File, Exists.Either)      => text("A file.")
      case (PathType.Directory, Exists.Either) => text("A directory.")
    }

    def typeName: String =
      pathType match {
        case PathType.Either    => "path"
        case PathType.File      => "file"
        case PathType.Directory => "directory"
      }

    def choices: Option[String] = None

    def validate(value: Option[String], conf: CLIConfig): IO[String, JPath] =
      (ZIO.fromOption(value) orElseFail "Path options do not have a default value").flatMap { value =>
        for {
          p <- fileSystem.parsePath(value)
          _ <- fileSystem.exists(p) >>= refineExistence(value, exists)
          _ <- ZIO.when(exists != Exists.No) {
                pathType match {
                  case Either => IO.unit
                  case File =>
                    ZIO.fail(s"Expected path '$value' to be a regular file.").unlessM(fileSystem.isRegularFile(p))
                  case Directory =>
                    ZIO.fail(s"Expected path '$value' to be a directory.").unlessM(fileSystem.isDirectory(p))
                }
              }
        } yield p
      }

    private def refineExistence(value: String, expected: Exists)(actual: Boolean): ZIO[Any, String, Unit] =
      (expected, actual) match {
        case (Exists.No, true)   => IO.fail(s"Path '$value' must not exist.")
        case (Exists.Yes, false) => IO.fail(s"Path '$value' must exist.")
        case _                   => IO.unit
      }
  }

  /**
   * Type representing a value selected from set of allowed values.
   * @param cases lists of allowed parameter-value pairs
   */
  final case class Enumeration[A](cases: (String, A)*) extends PrimType[A] {
    def helpDoc: HelpDoc.Span = text("One of the following cases: " + cases.map(_._1).mkString(", ") + ".")

    def typeName: String = "choice"

    def choices: Option[String] = Some(cases.map(_._1).mkString(" | "))

    def validate(value: Option[String], conf: CLIConfig): IO[String, A] =
      (ZIO.fromOption(value) orElseFail "Enumeration options do not have a default value.").flatMap { value =>
        cases.find(_._1 == value) match {
          case None         => IO.fail("Expected one of the following cases: " + cases.map(_._1).mkString(", "))
          case Some((_, a)) => IO.succeed(a)
        }
      }
  }

  /**
   * Type representing any text.
   */
  case object Text extends PrimType[String] {
    def typeName: String = "text"

    def choices: Option[String] = None

    def validate(value: Option[String], conf: CLIConfig): IO[String, String] =
      attempt(value, v => v, typeName)

    def helpDoc: HelpDoc.Span = text("A user defined piece of text.")
  }

  /**
   * Type representing decimal value via BigDecimal.
   */
  case object Decimal extends PrimType[BigDecimal] {
    def typeName: String = "decimal"

    def choices: Option[String] = None

    def validate(value: Option[String], conf: CLIConfig): IO[String, BigDecimal] =
      attempt(value, BigDecimal(_), typeName)

    def helpDoc: HelpDoc.Span = text("A decimal number.")
  }

  /**
   * Type representing integer value via BigInt.
   */
  case object Integer extends PrimType[BigInt] {
    def typeName: String = "integer"

    def choices: Option[String] = None

    def validate(value: Option[String], conf: CLIConfig): IO[String, BigInt] =
      attempt(value, BigInt(_), typeName)

    def helpDoc: HelpDoc.Span = text("An integer.")
  }

  /**
   * Type representing a boolean value.
   * True value can be passed as "true", "1", "y", "yes" or "on".
   * False value can be passed as "false", "o", "n", "no" or "off".
   * @param defaultValue Default value used then param is not provided
   */
  final case class Bool(defaultValue: Option[Boolean]) extends PrimType[Boolean] {
    def typeName: String = "boolean"

    def choices: Option[String] = Some("true | false")

    def validate(value: Option[String], conf: CLIConfig): IO[String, Boolean] =
      value.map(conf.normalizeCase) match {
        case Some(s) =>
          s match {
            case "true" | "1" | "y" | "yes" | "on"  => IO.succeed(true)
            case "false" | "0" | "n" | "no" | "off" => IO.succeed(false)
            case _                                  => IO.fail(s"$s cannot be recognized as valid boolean.")
          }
        case None => IO.succeed(defaultValue.get)
      }

    def helpDoc: HelpDoc.Span = text("A true or false value.")
  }

  /**
   * Type representing parameter for instant in time in UTC format, such as 2007-12-03T10:15:30.00Z.
   */
  case object Instant extends PrimType[JInstant] {
    def typeName: String = "instant"

    def choices: Option[String] = None

    def validate(value: Option[String], conf: CLIConfig): IO[String, JInstant] =
      attempt(value, JInstant.parse, typeName)

    def helpDoc: HelpDoc.Span = text("An instant in time in UTC format, such as 2007-12-03T10:15:30.00Z.")
  }

  /**
   * Type representing parameter for a date in ISO_LOCAL_DATE format, such as 2007-12-03.
   */
  case object LocalDate extends PrimType[JLocalDate] {
    def typeName: String = "date"

    def choices: Option[String] = None

    def validate(value: Option[String], conf: CLIConfig): IO[String, JLocalDate] =
      attempt(value, JLocalDate.parse, typeName)

    def helpDoc: HelpDoc.Span = text("A date in ISO_LOCAL_DATE format, such as 2007-12-03")
  }

  /**
   * Type representing a date-time without a time-zone in the ISO-8601 format, such as 2007-12-03T10:15:30.
   */
  case object LocalDateTime extends PrimType[JLocalDateTime] {
    def typeName: String = "date-time"

    def choices: Option[String] = None

    def validate(value: Option[String], conf: CLIConfig): IO[String, JLocalDateTime] =
      attempt(value, JLocalDateTime.parse, typeName)

    def helpDoc: HelpDoc.Span =
      text("A date-time without a time-zone in the ISO-8601 format, such as 2007-12-03T10:15:30.")
  }

  /**
   * Type representing a time without a time-zone in the ISO-8601 format, such as 10:15:30.
   */
  case object LocalTime extends PrimType[JLocalTime] {
    def typeName: String = "local-time"

    def choices: Option[String] = None

    def validate(value: Option[String], conf: CLIConfig): IO[String, JLocalTime] =
      attempt(value, JLocalTime.parse, typeName)

    def helpDoc: HelpDoc.Span = text("A time without a time-zone in the ISO-8601 format, such as 10:15:30.")
  }

  /**
   * Type representing a month-day in the ISO-8601 format such as 12-03.
   */
  case object MonthDay extends PrimType[JMonthDay] {
    def typeName: String = "month-day"

    def choices: Option[String] = None

    def validate(value: Option[String], conf: CLIConfig): IO[String, JMonthDay] =
      attempt(value, JMonthDay.parse, typeName)

    def helpDoc: HelpDoc.Span = text("A month-day in the ISO-8601 format such as 12-03.")
  }

  /**
   * Type representing a date-time with an offset from UTC/Greenwich in the ISO-8601 format, such as 2007-12-03T10:15:30+01:00.
   */
  case object OffsetDateTime extends PrimType[JOffsetDateTime] {
    def typeName: String = "offset-date-time"

    def choices: Option[String] = None

    def validate(value: Option[String], conf: CLIConfig): IO[String, JOffsetDateTime] =
      attempt(value, JOffsetDateTime.parse, typeName)

    def helpDoc: HelpDoc.Span =
      text("A date-time with an offset from UTC/Greenwich in the ISO-8601 format, such as 2007-12-03T10:15:30+01:00.")
  }

  /**
   * Type representing a time with an offset from UTC/Greenwich in the ISO-8601 format, such as 10:15:30+01:00.
   */
  case object OffsetTime extends PrimType[JOffsetTime] {
    def typeName: String = "offset-time"

    def choices: Option[String] = None

    def validate(value: Option[String], conf: CLIConfig): IO[String, JOffsetTime] =
      attempt(value, JOffsetTime.parse, typeName)

    def helpDoc: HelpDoc.Span =
      text("A time with an offset from UTC/Greenwich in the ISO-8601 format, such as 10:15:30+01:00.")
  }

  /**
   * Type representing a date-based amount of time in the ISO-8601 format, such as 'P1Y2M3D'.
   */
  case object Period extends PrimType[JPeriod] {
    def typeName: String = "period"

    def choices: Option[String] = None

    def validate(value: Option[String], conf: CLIConfig): IO[String, JPeriod] =
      attempt(value, JPeriod.parse, typeName)

    def helpDoc: HelpDoc.Span =
      text("A date-based amount of time in the ISO-8601 format, such as 'P1Y2M3D'.")
  }

  /**
   * Type representing a year in the ISO-8601 format, such as 2007.
   */
  case object Year extends PrimType[JYear] {
    def typeName: String = "year"

    def choices: Option[String] = None

    def validate(value: Option[String], conf: CLIConfig): IO[String, JYear] =
      attempt(value, s => JYear.of(s.toInt), typeName)

    def helpDoc: HelpDoc.Span = text("A year in the ISO-8601 format, such as 2007.")
  }

  /**
   * Type representing a year-month in the ISO-8601 format, such as 2007-12..
   */
  case object YearMonth extends PrimType[JYearMonth] {
    def typeName: String = "year-month"

    def choices: Option[String] = None

    def validate(value: Option[String], conf: CLIConfig): IO[String, JYearMonth] = {
      val AcceptedFormat = "^(-?\\d+)-(\\d{2})".r
      def parse(input: String) = input match {
        case AcceptedFormat(y, m) => IO.effect(JYearMonth.of(y.toInt, m.toInt))
        case _                    => IO.fail(())
      }

      (IO.fromOption(value) orElseFail "year-month does not have a default value").flatMap(parse) orElse IO.fail(
        s"${value.getOrElse("")} is not a ${typeName}."
      )
    }

    def helpDoc: HelpDoc.Span = text("A year-month in the ISO-8601 format, such as 2007-12.")
  }

  /**
   * Type representing a date-time with a time-zone in the ISO-8601 format, such as 2007-12-03T10:15:30+01:00 Europe/Paris.
   */
  case object ZonedDateTime extends PrimType[JZonedDateTime] {
    def typeName: String = "zoned-date-time"

    def choices: Option[String] = None

    def validate(value: Option[String], conf: CLIConfig): IO[String, JZonedDateTime] =
      attempt(value, JZonedDateTime.parse, typeName)

    def helpDoc: HelpDoc.Span =
      text("A date-time with a time-zone in the ISO-8601 format, such as 2007-12-03T10:15:30+01:00 Europe/Paris.")
  }

  /**
   * Type representing a time-zone ID, such as Europe/Paris.
   */
  case object ZoneId extends PrimType[JZoneId] {
    def typeName: String = "zone-id"

    def choices: Option[String] = None

    def validate(value: Option[String], conf: CLIConfig): IO[String, JZoneId] = attempt(value, JZoneId.of, typeName)

    def helpDoc: HelpDoc.Span = text("A time-zone ID, such as Europe/Paris.")
  }

  /**
   * Type representing a time-zone offset from Greenwich/UTC, such as +02:00.
   */
  case object ZoneOffset extends PrimType[JZoneOffset] {
    def typeName: String = "zone-offset"

    def choices: Option[String] = None

    def validate(value: Option[String], conf: CLIConfig): IO[String, JZoneOffset] =
      attempt(value, JZoneOffset.of, typeName)

    def helpDoc: HelpDoc.Span = text("A time-zone offset from Greenwich/UTC, such as +02:00.")
  }

  private def attempt[A, E](value: Option[String], parse: String => A, typeName: String): IO[String, A] =
    (ZIO.fromOption(value) orElseFail s"${typeName} options do not have a default value.").flatMap { value =>
      IO(parse(value)) orElseFail (s"${value} is not a ${typeName}.")
    }
}
