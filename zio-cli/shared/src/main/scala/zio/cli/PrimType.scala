package zio.cli

import java.nio.file.{ Path => JPath, Paths => JPaths, Files => JFiles }
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
  ZonedDateTime => JZonedDateTime,
  ZoneOffset => JZoneOffset,
  ZoneId => JZoneId
}

import zio._

/**
 * A `PrimType` represents the primitive types supported by ZIO CLI.
 *
 * Each primitive type has a way to parse and validate from a string.
 */
sealed trait PrimType[+A] {
  // TODO: Human-friendly rendering
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
    def validate(value: String): IO[String, JPath] =
      for {
        p <- IO.effect(JPaths.get(value)) orElseFail (s"'$value' is not a recognized path.")
        _ <- exists(p) >>= refineExistence(value, exists)
        _ <- pathType match {
              case Anything  => IO.unit
              case File      => ZIO.fail(s"Expected path '$value' to be a regular file.").unlessM(isRegularFile(p))
              case Directory => ZIO.fail(s"Expected path '$value' to be a directory.").unlessM(isDirectory(p))
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
  }

  case object Text extends PrimType[String] {
    def validate(value: String): IO[String, String] = attempt(value, _ => value, render)
  }

  case object Decimal extends PrimType[BigDecimal] {
    def validate(value: String): IO[String, BigDecimal] = attempt(value, BigDecimal(_), render)
  }

  case object Integer extends PrimType[BigInt] {
    def validate(value: String): IO[String, BigInt] = attempt(value, BigInt(_), render)
  }

  case object Boolean extends PrimType[Boolean] {
    def validate(value: String): IO[String, Boolean] = value.trim.toLowerCase match {
      case "true" | "1" | "y" | "yes" | "on"  => IO.succeed(true)
      case "false" | "0" | "n" | "no" | "off" => IO.succeed(false)
      case _                                  => IO.fail(s"$value cannot be recognized as valid boolean.")
    }
  }

  case object Instant extends PrimType[JInstant] {
    def validate(value: String): IO[String, JInstant] = attempt(value, JInstant.parse, render)
  }

  case object LocalDate extends PrimType[JLocalDate] {
    def validate(value: String): IO[String, JLocalDate] = attempt(value, JLocalDate.parse, render)
  }

  case object LocalDateTime extends PrimType[JLocalDateTime] {
    def validate(value: String): IO[String, JLocalDateTime] = attempt(value, JLocalDateTime.parse, render)
  }

  case object LocalTime extends PrimType[JLocalTime] {
    def validate(value: String): IO[String, JLocalTime] = attempt(value, JLocalTime.parse, render)
  }

  case object MonthDay extends PrimType[JMonthDay] {
    def validate(value: String): IO[String, JMonthDay] = attempt(value, JMonthDay.parse, render)
  }

  case object OffsetDateTime extends PrimType[JOffsetDateTime] {
    def validate(value: String): IO[String, JOffsetDateTime] = attempt(value, JOffsetDateTime.parse, render)
  }

  case object OffsetTime extends PrimType[JOffsetTime] {
    def validate(value: String): IO[String, JOffsetTime] = attempt(value, JOffsetTime.parse, render)
  }

  case object Period extends PrimType[JPeriod] {
    def validate(value: String): IO[String, JPeriod] = attempt(value, JPeriod.parse, render)
  }

  case object Year extends PrimType[JYear] {
    def validate(value: String): IO[String, JYear] = attempt(value, s => JYear.of(s.toInt), render)
  }

  case object YearMonth extends PrimType[JYearMonth] {
    def validate(value: String): IO[String, JYearMonth] = {
      val AcceptedFormat = "^(-?\\d+)-(\\d{2})".r
      def parse(input: String) = input match {
        case AcceptedFormat(y, m) => IO.effect(JYearMonth.of(y.toInt, m.toInt))
        case _                    => IO.fail(())
      }

      parse(value) orElse IO.fail(s"${value} is not a ${render}.")
    }
  }

  case object ZonedDateTime extends PrimType[JZonedDateTime] {
    def validate(value: String): IO[String, JZonedDateTime] = attempt(value, JZonedDateTime.parse, render)
  }

  case object ZoneId extends PrimType[JZoneId] {
    def validate(value: String): IO[String, JZoneId] = attempt(value, JZoneId.of, render)
  }
  case object ZoneOffset extends PrimType[JZoneOffset] {
    def validate(value: String): IO[String, JZoneOffset] = attempt(value, JZoneOffset.of, render)
  }

  private def attempt[A, E](value: String, parse: String => A, typeName: String): IO[String, A] =
    IO(parse(value)) orElseFail (s"${value} is not a ${typeName}.")
}
