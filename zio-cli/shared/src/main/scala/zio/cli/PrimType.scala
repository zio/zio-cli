package zio.cli

import java.nio.file.{Files => JFiles, Path => JPath, Paths => JPaths}
import java.time.{Instant => JInstant, LocalDate => JLocalDate, LocalDateTime => JLocalDateTime, LocalTime => JLocalTime, MonthDay => JMonthDay, OffsetDateTime => JOffsetDateTime, OffsetTime => JOffsetTime, Period => JPeriod, Year => JYear, YearMonth => JYearMonth, ZoneId => JZoneId, ZoneOffset => JZoneOffset, ZonedDateTime => JZonedDateTime}

import zio._

/**
 * A `PrimType` represents the primitive types supported by ZIO CLI.
 *
 * Each primitive type has a way to parse and validate from a string.
 */
sealed trait PrimType[+A] {
  // TODO: Human-friendly rendering
  def helpDoc: HelpDoc.Span

  def render: String = toString()

  def validate(value: String): IO[String, A]
}

object PrimType {
  sealed trait PathType
  object PathType {
    case object Anything  extends PathType
    case object File      extends PathType
    case object Directory extends PathType
  }

  final case class Path(pathType: PathType, exists: Boolean) extends PrimType[JPath] {
    import PathType._
    import HelpDoc.dsl._
    def validate(value: String): IO[String, JPath] =
      for {
        p <- IO.effect(JPaths.get(value)) orElseFail (s"'$value' is not a recognized path.")
        _ <- exists(p) >>= refineExistence(value, exists)
        _ <- ZIO.when(exists) {
              pathType match {
                case Anything  => IO.unit
                case File      => ZIO.fail(s"Expected path '$value' to be a regular file.").unlessM(isRegularFile(p))
                case Directory => ZIO.fail(s"Expected path '$value' to be a directory.").unlessM(isDirectory(p))
              }
            }
      } yield p

    private def exists(path: JPath) = IO.effect(JFiles.exists(path)) orElse IO.succeed(false)

    private def isDirectory(path: JPath) = IO.effect(JFiles.isDirectory(path)) orElse IO.succeed(false)

    private def isRegularFile(path: JPath) = IO.effect(JFiles.isRegularFile(path)) orElse IO.succeed(false)

    private def refineExistence(value: String, expected: Boolean)(actual: Boolean) =
      (expected, actual) match {
        case (true, false) => IO.fail(s"Path '$value' does not exist.")
        case (false, true) => IO.fail(s"Path '$value' expected to not exist, but it does.")
        case _             => IO.unit
      }

    override def helpDoc: HelpDoc.Span = (pathType, exists) match {
      case (PathType.Anything, true) => text("An existing file or directory.")
      case (PathType.File, true) => text("An existing file.")
      case (PathType.Directory, true) => text("An existing directory.")
      case (PathType.Anything, false) => text("A file or directory that must not exist.")
      case (PathType.File, false) => text("A file that does not exist.")
      case (PathType.Directory, false) => text("A directory that does not exist.")
    }
  }

  case object Text extends PrimType[String] {
    import HelpDoc.dsl._
    def validate(value: String): IO[String, String] = attempt(value, _ => value, render)

    override def helpDoc: HelpDoc.Span = text("A user defined piece of text.")
  }

  case object Decimal extends PrimType[BigDecimal] {
    import HelpDoc.dsl._
    def validate(value: String): IO[String, BigDecimal] = attempt(value, BigDecimal(_), render)

    override def helpDoc: HelpDoc.Span = text("A decimal number.")
  }

  case object Integer extends PrimType[BigInt] {
    import HelpDoc.dsl._
    def validate(value: String): IO[String, BigInt] = attempt(value, BigInt(_), render)

    override def helpDoc: HelpDoc.Span = text("An integer.")
  }

  case object Boolean extends PrimType[Boolean] {
    import HelpDoc.dsl._
    def validate(value: String): IO[String, Boolean] = value.trim.toLowerCase match {
      case "true" | "1" | "y" | "yes" | "on"  => IO.succeed(true)
      case "false" | "0" | "n" | "no" | "off" => IO.succeed(false)
      case _                                  => IO.fail(s"$value cannot be recognized as valid boolean.")
    }

    override def helpDoc: HelpDoc.Span = text("A true or false value.")
  }

  case object Instant extends PrimType[JInstant] {
    import HelpDoc.dsl._
    def validate(value: String): IO[String, JInstant] = attempt(value, JInstant.parse, render)

    override def helpDoc: HelpDoc.Span = text("An instant in time in UTC format, such as 2007-12-03T10:15:30.00Z.")
  }

  case object LocalDate extends PrimType[JLocalDate] {
    import HelpDoc.dsl._
    def validate(value: String): IO[String, JLocalDate] = attempt(value, JLocalDate.parse, render)

    override def helpDoc: HelpDoc.Span = text("A date in ISO_LOCAL_DATE format, such as 2007-12-03")
  }

  case object LocalDateTime extends PrimType[JLocalDateTime] {
    import HelpDoc.dsl._
    def validate(value: String): IO[String, JLocalDateTime] = attempt(value, JLocalDateTime.parse, render)

    override def helpDoc: HelpDoc.Span = text("A date-time without a time-zone in the ISO-8601 format, such as 2007-12-03T10:15:30.")
  }

  case object LocalTime extends PrimType[JLocalTime] {
    import HelpDoc.dsl._
    def validate(value: String): IO[String, JLocalTime] = attempt(value, JLocalTime.parse, render)

    override def helpDoc: HelpDoc.Span = text("A time without a time-zone in the ISO-8601 format, such as 10:15:30.")
  }

  case object MonthDay extends PrimType[JMonthDay] {
    import HelpDoc.dsl._
    def validate(value: String): IO[String, JMonthDay] = attempt(value, JMonthDay.parse, render)

    override def helpDoc: HelpDoc.Span = text("A month-day in the ISO-8601 format such as 12-03.")
  }

  case object OffsetDateTime extends PrimType[JOffsetDateTime] {
    import HelpDoc.dsl._
    def validate(value: String): IO[String, JOffsetDateTime] = attempt(value, JOffsetDateTime.parse, render)

    override def helpDoc: HelpDoc.Span = text("A date-time with an offset from UTC/Greenwich in the ISO-8601 format, such as 2007-12-03T10:15:30+01:00.")
  }

  case object OffsetTime extends PrimType[JOffsetTime] {
    import HelpDoc.dsl._
    def validate(value: String): IO[String, JOffsetTime] = attempt(value, JOffsetTime.parse, render)

    override def helpDoc: HelpDoc.Span = text("A time with an offset from UTC/Greenwich in the ISO-8601 format, such as 10:15:30+01:00}.")
  }

  case object Period extends PrimType[JPeriod] {
    import HelpDoc.dsl._
    def validate(value: String): IO[String, JPeriod] = attempt(value, JPeriod.parse, render)

    override def helpDoc: HelpDoc.Span = text("A date-based amount of time in the ISO-8601 format, such as '2 years, 3 months and 4 days'.")
  }

  case object Year extends PrimType[JYear] {
    import HelpDoc.dsl._
    def validate(value: String): IO[String, JYear] = attempt(value, s => JYear.of(s.toInt), render)

    override def helpDoc: HelpDoc.Span = text("A year in the ISO-8601 format, such as 2007.")
  }

  case object YearMonth extends PrimType[JYearMonth] {
    import HelpDoc.dsl._
    def validate(value: String): IO[String, JYearMonth] = {
      val AcceptedFormat = "^(-?\\d+)-(\\d{2})".r
      def parse(input: String) = input match {
        case AcceptedFormat(y, m) => IO.effect(JYearMonth.of(y.toInt, m.toInt))
        case _                    => IO.fail(())
      }

      parse(value) orElse IO.fail(s"${value} is not a ${render}.")
    }

    override def helpDoc: HelpDoc.Span = text("A year-month in the ISO-8601 format, such as 2007-12.")
  }

  case object ZonedDateTime extends PrimType[JZonedDateTime] {
    import HelpDoc.dsl._
    def validate(value: String): IO[String, JZonedDateTime] = attempt(value, JZonedDateTime.parse, render)

    override def helpDoc: HelpDoc.Span = text("A date-time with a time-zone in the ISO-8601 format, such as 2007-12-03T10:15:30+01:00 Europe/Paris.")
  }

  case object ZoneId extends PrimType[JZoneId] {
    import HelpDoc.dsl._
    def validate(value: String): IO[String, JZoneId] = attempt(value, JZoneId.of, render)

    override def helpDoc: HelpDoc.Span = text("A time-zone ID, such as Europe/Paris.")
  }
  case object ZoneOffset extends PrimType[JZoneOffset] {
    import HelpDoc.dsl._
    def validate(value: String): IO[String, JZoneOffset] = attempt(value, JZoneOffset.of, render)

    override def helpDoc: HelpDoc.Span = text("A time-zone offset from Greenwich/UTC, such as +02:00.")
  }

  private def attempt[A, E](value: String, parse: String => A, typeName: String): IO[String, A] =
    IO(parse(value)) orElseFail (s"${value} is not a ${typeName}.")
}
