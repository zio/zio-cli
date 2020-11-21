package zio.cli

import java.nio.file.{ Files => JFiles, Path => JPath, Paths => JPaths }
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
import HelpDoc.Span.text
import zio.cli.PathType.File
import zio.cli.PathType.Directory

/**
 * A `PrimType` represents the primitive types supported by ZIO CLI.
 *
 * Each primitive type has a way to parse and validate from a string.
 */
sealed trait PrimType[+A] {
  def helpDoc: HelpDoc.Span

  def typeName: String

  def choices: Option[String]

  final def validate(value: String, opts: ParserOptions): IO[String, A] = validate(Option(value))

  def validate(value: Option[String], opts: ParserOptions): IO[String, A]
}

object PrimType {
  final case class Path(pathType: PathType, exists: Exists) extends PrimType[JPath] {
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

    def validate(value: Option[String], opts: ParserOptions): IO[String, JPath] =
      (ZIO.fromOption(value) orElseFail "Path options do not have a default value").flatMap { value =>
        for {
          p <- IO.effect(JPaths.get(value)) orElseFail (s"'$value' is not a recognized path.")
          _ <- exists(p) >>= refineExistence(value, exists)
          _ <- ZIO.when(exists != Exists.No) {
                pathType match {
                  case Either    => IO.unit
                  case File      => ZIO.fail(s"Expected path '$value' to be a regular file.").unlessM(isRegularFile(p))
                  case Directory => ZIO.fail(s"Expected path '$value' to be a directory.").unlessM(isDirectory(p))
                }
              }
        } yield p
      }

    private def exists(path: JPath) = IO.effect(JFiles.exists(path)) orElse IO.succeed(false)

    private def isDirectory(path: JPath) = IO.effect(JFiles.isDirectory(path)) orElse IO.succeed(false)

    private def isRegularFile(path: JPath) = IO.effect(JFiles.isRegularFile(path)) orElse IO.succeed(false)

    private def refineExistence(value: String, expected: Exists)(actual: Boolean) =
      (expected, actual) match {
        case (Exists.No, true)   => IO.fail(s"Path '$value' must not exist.")
        case (Exists.Yes, false) => IO.fail(s"Path '$value' must exist.")
        case _                   => IO.unit
      }
  }

  final case class Enumeration[A](cases: (String, A)*) extends PrimType[A] {
    def helpDoc: HelpDoc.Span = text("One of the following cases: " + cases.map(_._1).mkString(", ") + ".")

    def typeName: String = "choice"

    def choices: Option[String] = Some(cases.map(_._1).mkString(" | "))

    def validate(value: Option[String], opts: ParserOptions): IO[String, A] =
      (ZIO.fromOption(value) orElseFail "Enumeration options do not have a default value.").flatMap { value =>
        cases.find(_._1 == value) match {
          case None         => IO.fail("Expected one of the following cases: " + cases.map(_._1).mkString(", "))
          case Some((_, a)) => IO.succeed(a)
        }
      }
  }

  case object Text extends PrimType[String] {
    def typeName: String = "text"

    def choices: Option[String] = None

    def validate(value: Option[String], opts: ParserOptions): IO[String, String] = attempt(value, v => v, typeName)

    def helpDoc: HelpDoc.Span = text("A user defined piece of text.")
  }

  case object Decimal extends PrimType[BigDecimal] {
    def typeName: String = "decimal"

    def choices: Option[String] = None

    def validate(value: Option[String], opts: ParserOptions): IO[String, BigDecimal] = attempt(value, BigDecimal(_), typeName)

    def helpDoc: HelpDoc.Span = text("A decimal number.")
  }

  case object Integer extends PrimType[BigInt] {
    def typeName: String = "integer"

    def choices: Option[String] = None

    def validate(value: Option[String], opts: ParserOptions): IO[String, BigInt] = attempt(value, BigInt(_), typeName)

    def helpDoc: HelpDoc.Span = text("An integer.")
  }

  final case class Bool(defaultValue: Option[Boolean]) extends PrimType[Boolean] {
    def typeName: String = "boolean"

    def choices: Option[String] = Some("true | false")

    def validate(value: Option[String], opts: ParserOptions): IO[String, Boolean] = value match {
      case Some(value) =>
        value match {
          case "true" | "1" | "y" | "yes" | "on"  => IO.succeed(true)
          case "false" | "0" | "n" | "no" | "off" => IO.succeed(false)
          case _                                  => IO.fail(s"$value cannot be recognized as valid boolean.")
        }
      case None => IO.succeed(defaultValue.get)
    }

    def helpDoc: HelpDoc.Span = text("A true or false value.")
  }

  case object Instant extends PrimType[JInstant] {
    def typeName: String = "instant"

    def choices: Option[String] = None

    def validate(value: Option[String], opts: ParserOptions): IO[String, JInstant] = attempt(value, JInstant.parse, typeName)

    def helpDoc: HelpDoc.Span = text("An instant in time in UTC format, such as 2007-12-03T10:15:30.00Z.")
  }

  case object LocalDate extends PrimType[JLocalDate] {
    def typeName: String = "date"

    def choices: Option[String] = None

    def validate(value: Option[String], opts: ParserOptions): IO[String, JLocalDate] = attempt(value, JLocalDate.parse, typeName)

    def helpDoc: HelpDoc.Span = text("A date in ISO_LOCAL_DATE format, such as 2007-12-03")
  }

  case object LocalDateTime extends PrimType[JLocalDateTime] {
    def typeName: String = "date-time"

    def choices: Option[String] = None

    def validate(value: Option[String], opts: ParserOptions): IO[String, JLocalDateTime] = attempt(value, JLocalDateTime.parse, typeName)

    def helpDoc: HelpDoc.Span =
      text("A date-time without a time-zone in the ISO-8601 format, such as 2007-12-03T10:15:30.")
  }

  case object LocalTime extends PrimType[JLocalTime] {
    def typeName: String = "local-time"

    def choices: Option[String] = None

    def validate(value: Option[String], opts: ParserOptions): IO[String, JLocalTime] = attempt(value, JLocalTime.parse, typeName)

    def helpDoc: HelpDoc.Span = text("A time without a time-zone in the ISO-8601 format, such as 10:15:30.")
  }

  case object MonthDay extends PrimType[JMonthDay] {
    def typeName: String = "month-day"

    def choices: Option[String] = None

    def validate(value: Option[String], opts: ParserOptions): IO[String, JMonthDay] = attempt(value, JMonthDay.parse, typeName)

    def helpDoc: HelpDoc.Span = text("A month-day in the ISO-8601 format such as 12-03.")
  }

  case object OffsetDateTime extends PrimType[JOffsetDateTime] {
    def typeName: String = "offset-date-time"

    def choices: Option[String] = None

    def validate(value: Option[String], opts: ParserOptions): IO[String, JOffsetDateTime] = attempt(value, JOffsetDateTime.parse, typeName)

    def helpDoc: HelpDoc.Span =
      text("A date-time with an offset from UTC/Greenwich in the ISO-8601 format, such as 2007-12-03T10:15:30+01:00.")
  }

  case object OffsetTime extends PrimType[JOffsetTime] {
    def typeName: String = "offset-time"

    def choices: Option[String] = None

    def validate(value: Option[String], opts: ParserOptions): IO[String, JOffsetTime] = attempt(value, JOffsetTime.parse, typeName)

    def helpDoc: HelpDoc.Span =
      text("A time with an offset from UTC/Greenwich in the ISO-8601 format, such as 10:15:30+01:00}.")
  }

  case object Period extends PrimType[JPeriod] {
    def typeName: String = "period"

    def choices: Option[String] = None

    def validate(value: Option[String], opts: ParserOptions): IO[String, JPeriod] = attempt(value, JPeriod.parse, typeName)

    def helpDoc: HelpDoc.Span =
      text("A date-based amount of time in the ISO-8601 format, such as 'P1Y2M3D'.")
  }

  case object Year extends PrimType[JYear] {
    def typeName: String = "year"

    def choices: Option[String] = None

    def validate(value: Option[String], opts: ParserOptions): IO[String, JYear] = attempt(value, s => JYear.of(s.toInt), typeName)

    def helpDoc: HelpDoc.Span = text("A year in the ISO-8601 format, such as 2007.")
  }

  case object YearMonth extends PrimType[JYearMonth] {
    def typeName: String = "year-month"

    def choices: Option[String] = None

    def validate(value: Option[String], opts: ParserOptions): IO[String, JYearMonth] = {
      val AcceptedFormat = "^(-?\\d+)-(\\d{2})".r
      def parse(input: String) = input match {
        case AcceptedFormat(y, m) => IO.effect(JYearMonth.of(y.toInt, m.toInt))
        case _                    => IO.fail(())
      }

      (IO.fromOption(value) orElseFail "year-month does not have a default value").flatMap(parse) orElse IO.fail(
        s"${value} is not a ${typeName}."
      )
    }

    def helpDoc: HelpDoc.Span = text("A year-month in the ISO-8601 format, such as 2007-12.")
  }

  case object ZonedDateTime extends PrimType[JZonedDateTime] {
    def typeName: String = "zoned-date-time"

    def choices: Option[String] = None

    def validate(value: Option[String], opts: ParserOptions): IO[String, JZonedDateTime] = attempt(value, JZonedDateTime.parse, typeName)

    def helpDoc: HelpDoc.Span =
      text("A date-time with a time-zone in the ISO-8601 format, such as 2007-12-03T10:15:30+01:00 Europe/Paris.")
  }

  case object ZoneId extends PrimType[JZoneId] {
    def typeName: String = "zone-id"

    def choices: Option[String] = None

    def validate(value: Option[String], opts: ParserOptions): IO[String, JZoneId] = attempt(value, JZoneId.of, typeName)

    def helpDoc: HelpDoc.Span = text("A time-zone ID, such as Europe/Paris.")
  }
  case object ZoneOffset extends PrimType[JZoneOffset] {
    def typeName: String = "zone-offset"

    def choices: Option[String] = None

    def validate(value: Option[String], opts: ParserOptions): IO[String, JZoneOffset] = attempt(value, JZoneOffset.of, typeName)

    def helpDoc: HelpDoc.Span = text("A time-zone offset from Greenwich/UTC, such as +02:00.")
  }

  private def attempt[A, E](value: Option[String], parse: String => A, typeName: String): IO[String, A] =
    (ZIO.fromOption(value) orElseFail s"${typeName} options do not have a default value.").flatMap { value =>
      IO(parse(value)) orElseFail (s"${value} is not a ${typeName}.")
    }
}
