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
  case object Text                                           extends PrimType[String]
  case object Decimal                                        extends PrimType[BigDecimal]
  case object Integer                                        extends PrimType[BigInt]
  case object Instant                                        extends PrimType[JInstant]
  case object LocalDate                                      extends PrimType[JLocalDate]
  case object LocalDateTime                                  extends PrimType[JLocalDateTime]
  case object LocalTime                                      extends PrimType[JLocalTime]
  case object MonthDay                                       extends PrimType[JMonthDay]
  case object OffsetDateTime                                 extends PrimType[JOffsetDateTime]
  case object OffsetTime                                     extends PrimType[JOffsetTime]
  case object Period                                         extends PrimType[JPeriod]
  case object Year                                           extends PrimType[JYear]
  case object YearMonth                                      extends PrimType[JYearMonth]
  case object ZonedDateTime                                  extends PrimType[JZonedDateTime]
  case object ZoneId                                         extends PrimType[JZoneId]
  case object ZoneOffset                                     extends PrimType[JZoneOffset]
}
