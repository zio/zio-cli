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

/**
 * A `Flag[A]` models a command-line flag that produces a value of type `A`.
 */
sealed trait Flags[+A] { self =>

  def ::[That, A1 >: A](that: Flags[That]): Flags.Cons[That, A1] =
    Flags.Cons(that, self)
}

object Flags {

  final case object Empty extends Flags[Unit]

  final case class Single[+A](name: String, aliases: Vector[String], flagType: Flags.Type[A]) extends Flags[A] {
    import Flags.Type._

    def ? : Flags[Option[A]] = optional

    def alias(name: String): Flags[A] = copy(aliases = aliases :+ name)

    def aliases(names: String*): Flags[A] = copy(aliases = aliases ++ names)

    def map[B](f: A => B): Flags[B] = copy(flagType = Map(flagType, f))

    def optional: Flags[Option[A]] = copy(flagType = Optional(flagType))
  }

  final case class Cons[A, B](left: Flags[A], right: Flags[B]) extends Flags[(A, B)]

  sealed trait Type[+A]
  object Type {

    sealed trait PathType

    object PathType {
      case object File      extends PathType
      case object Directory extends PathType
    }

    final case class Toggle(negationName: Option[String], ifPresent: Boolean) extends Type[Boolean]
    final case class Map[A, B](value: Type[A], f: A => B)                     extends Type[B]
    final case class Optional[A](value: Type[A])                              extends Type[Option[A]]
    final case class Path(pathType: PathType, createIfNotExists: Boolean)     extends Type[JPath]
    case object Present                                                       extends Type[Unit]
    case object Text                                                          extends Type[String]
    case object Decimal                                                       extends Type[BigDecimal]
    case object Integer                                                       extends Type[BigInt]
    case object Instant                                                       extends Type[JInstant]
    case object LocalDate                                                     extends Type[JLocalDate]
    case object LocalDateTime                                                 extends Type[JLocalDateTime]
    case object LocalTime                                                     extends Type[JLocalTime]
    case object MonthDay                                                      extends Type[JMonthDay]
    case object OffsetDateTime                                                extends Type[JOffsetDateTime]
    case object OffsetTime                                                    extends Type[JOffsetTime]
    case object Period                                                        extends Type[JPeriod]
    case object Year                                                          extends Type[JYear]
    case object YearMonth                                                     extends Type[JYearMonth]
    case object ZonedDateTime                                                 extends Type[JZonedDateTime]
    case object ZoneId                                                        extends Type[JZoneId]
    case object ZoneOffset                                                    extends Type[JZoneOffset]
  }

  /**
   * Creates a boolean flag with the specified name, which, if present, will
   * produce the specified constant boolean value.
   */
  def bool(name: String, ifPresent: Boolean, negationName: Option[String] = None): Single[Boolean] =
    Single(name, Vector.empty, Type.Toggle(negationName, ifPresent))

  def file(name: String, createIfNotExists: Boolean): Single[JPath] =
    Single(name, Vector.empty, Type.Path(Type.PathType.File, createIfNotExists))

  def directory(name: String, createIfNotExists: Boolean): Single[JPath] =
    Single(name, Vector.empty, Type.Path(Type.PathType.Directory, createIfNotExists))

  def text(name: String): Single[String] =
    Single(name, Vector.empty, Type.Text)

  def decimal(name: String): Single[BigDecimal] =
    Single(name, Vector.empty, Type.Decimal)

  def integer(name: String): Single[BigInt] =
    Single(name, Vector.empty, Type.Integer)

  def instant(name: String): Single[JInstant] =
    Single(name, Vector.empty, Type.Instant)

  def localDate(name: String): Single[JLocalDate] =
    Single(name, Vector.empty, Type.LocalDate)

  def localDateTime(name: String): Single[JLocalDateTime] =
    Single(name, Vector.empty, Type.LocalDateTime)

  def localTime(name: String): Single[JLocalTime] =
    Single(name, Vector.empty, Type.LocalTime)

  def monthDay(name: String): Single[JMonthDay] =
    Single(name, Vector.empty, Type.MonthDay)

  def offsetDateTime(name: String): Single[JOffsetDateTime] =
    Single(name, Vector.empty, Type.OffsetDateTime)

  def offsetTime(name: String): Single[JOffsetTime] =
    Single(name, Vector.empty, Type.OffsetTime)

  def period(name: String): Single[JPeriod] =
    Single(name, Vector.empty, Type.Period)

  def year(name: String): Single[JYear] =
    Single(name, Vector.empty, Type.Year)

  def yearMonth(name: String): Single[JYearMonth] =
    Single(name, Vector.empty, Type.YearMonth)

  def zonedDateTime(name: String): Single[JZonedDateTime] =
    Single(name, Vector.empty, Type.ZonedDateTime)

  def zoneId(name: String): Single[JZoneId] =
    Single(name, Vector.empty, Type.ZoneId)

  def zoneOffset(name: String): Single[JZoneOffset] =
    Single(name, Vector.empty, Type.ZoneOffset)

  val empty: Flags[Unit] = Empty
}
