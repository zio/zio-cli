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
import scala.util.{Try, Success, Failure}
import zio.cli.PrimType.PathType.Anything
import zio.cli.PrimType.PathType.File
import zio.cli.PrimType.PathType.Directory

/**
 * A `PrimType` represents the primitive types supported by ZIO CLI.
 *
 * Each primitive type has a way to parse and validate from a string.
 */
sealed trait PrimType[+A] {
  // TODO: Human-friendly rendering
  def render: String = toString()

  def validate(value: String): IO[String, A] = ???
}
object PrimType {
  sealed trait PathType
  object PathType {
    case object Anything  extends PathType
    case object File      extends PathType
    case object Directory extends PathType
  }

  final case class Path(pathType: PathType, exists: Boolean) extends PrimType[JPath]
  {
    import PathType._
    def validate(value: String): IO[String, JPath] = 
      for {
        p       <- IO.effect(JPaths.get(value)).catchAll(_ => IO.fail(s"Couldn't recognize '$value' as valid path."))
        _       <- exists(p) >>= refine("Expected path '$value' to exist.")
        _       <- pathType match {
                      case Anything => IO.unit
                      case File => isRegularFile(p) >>= refine(s"Expected path '$value' to be a regular file.")
                      case Directory => isDirectory(p) >>= refine(s"Expected path '$value' to be a directory.")
                    }
      } yield p
      
    private def exists(path: JPath) = IO.effect(JFiles.exists(path)) orElse IO.succeed(false)

    private def isDirectory(path: JPath) = IO.effect(JFiles.isDirectory(path)) orElse IO.succeed(false)

    private def isRegularFile(path: JPath) = IO.effect(JFiles.isDirectory(path)) orElse IO.succeed(false)

    private def refine(message: String) (f: Boolean) = if (f) IO.unit else IO.fail(message)
  }
  
  case object Text extends PrimType[String] {
    def validate(value: String): IO[String, String] = attempt(value, _ => value, "text")
  }
  
  case object Decimal extends PrimType[BigDecimal] {
    def validate(value: String): IO[String, BigDecimal] = attempt(value, BigDecimal(_), "decimal")
  }
  
  case object Integer extends PrimType[BigInt] {
    def validate(value: String): IO[String, BigInt] = attempt(value, BigInt(_), "integer")
  }

  case object Boolean extends PrimType[Boolean] {
    def validate(value: String): IO[String, Boolean] = value match {
      case "true"  | "1" | "y" | "yes" | "on"  => IO.succeed(true)
      case "false" | "0" | "n" | "no"  | "off" => IO.succeed(false)
      case s => IO.fail(s"Couldn't parse boolean from value: $value.")
    }
  }
  
  case object Instant extends PrimType[JInstant] {
    def validate(value: String): IO[String, JInstant] = attempt(value, JInstant.parse, "instant")
  }

  case object LocalDate extends PrimType[JLocalDate] {
    def validate(value: String): IO[String, JLocalDate] = attempt(value, JLocalDate.parse, "localdate")
  }

  case object LocalDateTime extends PrimType[JLocalDateTime] {
    def validate(value: String): IO[String, JLocalDateTime] = attempt(value, JLocalDateTime.parse, "localdatetime")
  }
  
  case object LocalTime extends PrimType[JLocalTime] {
    def validate(value: String): IO[String, JLocalTime] = attempt(value, JLocalTime.parse, "localtime")
  }

  case object MonthDay extends PrimType[JMonthDay] {
    def validate(value: String): IO[String, JMonthDay] = attempt(value, JMonthDay.parse, "monthday")
  }

  case object OffsetDateTime extends PrimType[JOffsetDateTime] {
    def validate(value: String): IO[String, JOffsetDateTime] = attempt(value, JOffsetDateTime.parse, "offsetdatetime")
  }
  
  case object OffsetTime extends PrimType[JOffsetTime] {
    def validate(value: String): IO[String, JOffsetTime] = attempt(value, JOffsetTime.parse, "offsettime")
  }
  
  case object Period extends PrimType[JPeriod] {
    def validate(value: String): IO[String, JPeriod] = attempt(value, JPeriod.parse, "period")
  }
  
  case object Year extends PrimType[JYear] {
    def validate(value: String): IO[String, JYear] = attempt(value, JYear.parse, "year")
  }
  
  case object YearMonth extends PrimType[JYearMonth] {
    def validate(value: String): IO[String, JYearMonth] = attempt(value, JYearMonth.parse, "yearmonth")
  }
  case object ZonedDateTime extends PrimType[JZonedDateTime] {
    def validate(value: String): IO[String, JZonedDateTime] = attempt(value, JZonedDateTime.parse, "zoneddatetime")
  }

  case object ZoneId extends PrimType[JZoneId] {
    def validate(value: String): IO[String, JZoneId] = attempt(value, JZoneId.of, "zoneid")
  }
  case object ZoneOffset extends PrimType[JZoneOffset] {
    def validate(value: String): IO[String, JZoneOffset] = attempt(value, JZoneOffset.of, "zoneoffset")
  }

  private def attempt[A, E](value: String, parse: String => A, typeName: String): IO[String, A] =
    Try(parse(value)) match {
      case Success(value)     => IO.succeed(value)
      case Failure(throwable) => IO.fail(s"Couldn't parse $typeName from value: $value.")
    }
}
