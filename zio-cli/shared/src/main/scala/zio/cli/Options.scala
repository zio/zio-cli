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

import zio.IO
import zio.cli.HelpDoc.dsl.{ error, p }
import scala.collection.immutable.Nil

/**
 * A `Flag[A]` models a command-line flag that produces a value of type `A`.
 */
sealed trait Options[+A] { self =>

  final def ::[That, A1 >: A](that: Options[That]): Options[(That, A1)] =
    Options.Cons(that, self)

  final def as[B, C, Z](f: (B, C) => Z)(implicit ev: A <:< ((B, C))): Options[Z] =
    self.map(ev).map { case ((b, c)) => f(b, c) }

  final def as[B, C, D, Z](f: (B, C, D) => Z)(implicit ev: A <:< ((B, (C, D)))): Options[Z] =
    self.map(ev).map { case ((b, (c, d))) => f(b, c, d) }

  final def as[B, C, D, E, Z](f: (B, C, D, E) => Z)(implicit ev: A <:< ((B, (C, (D, E))))): Options[Z] =
    self.map(ev).map { case ((b, (c, (d, e)))) => f(b, c, d, e) }

  final def as[B, C, D, E, F, Z](f0: (B, C, D, E, F) => Z)(implicit ev: A <:< ((B, (C, (D, (E, F)))))): Options[Z] =
    self.map(ev).map { case ((b, (c, (d, (e, f))))) => f0(b, c, d, e, f) }

  final def as[B, C, D, E, F, G, Z](
    f0: (B, C, D, E, F, G) => Z
  )(implicit ev: A <:< ((B, (C, (D, (E, (F, G))))))): Options[Z] =
    self.map(ev).map { case ((b, (c, (d, (e, (f, g)))))) => f0(b, c, d, e, f, g) }

  final def flatten2[B, C](implicit ev: A <:< ((B, C))): Options[(B, C)] = as[B, C, (B, C)]((_, _))

  final def flatten3[B, C, D](implicit ev: A <:< ((B, (C, D)))): Options[(B, C, D)] = as[B, C, D, (B, C, D)]((_, _, _))

  final def flatten4[B, C, D, E](implicit ev: A <:< ((B, (C, (D, E))))): Options[(B, C, D, E)] =
    as[B, C, D, E, (B, C, D, E)]((_, _, _, _))

  final def flatten5[B, C, D, E, F](implicit ev: A <:< ((B, (C, (D, (E, F)))))): Options[(B, C, D, E, F)] =
    as[B, C, D, E, F, (B, C, D, E, F)]((_, _, _, _, _))

  final def flatten6[B, C, D, E, F, G](implicit ev: A <:< ((B, (C, (D, (E, (F, G))))))): Options[(B, C, D, E, F, G)] =
    as[B, C, D, E, F, G, (B, C, D, E, F, G)]((_, _, _, _, _, _))

  final def helpDoc: HelpDoc.Block = ???

  final def map[B](f: A => B): Options[B] = Options.Map(self, (a: A) => Right(f(a)))

  final def mapTry[B](f: A => B): Options[B] =
    Options.Map(self, (a: A) => scala.util.Try(f(a)).toEither.left.map(e => p(error(e.getMessage))))

  def recognizes(value: String, opts: ParserOptions): Option[Int]

  final def requires[B](that: Options[B], suchThat: B => Boolean = (_: B) => true): Options[A] =
    Options.Requires(self, that, suchThat)

  final def requiresNot[B](that: Options[B], suchThat: B => Boolean = (_: B) => true): Options[A] =
    Options.RequiresNot(self, that, suchThat)

  def validate(args: List[String], opts: ParserOptions): IO[List[HelpDoc.Block], (List[String], A)]
}

object Options {
  // --verbose 3
  case object Empty extends Options[Unit] {
    def recognizes(value: String, opts: ParserOptions): Option[Int] = None

    def validate(args: List[String], opts: ParserOptions): IO[List[HelpDoc.Block], (List[String], Unit)] =
      IO.succeed((args, ()))
  }

  final case class Single[+A](
    name: String,
    aliases: Vector[String],
    optionType: Options.Type[A],
    description: Vector[String]
  ) extends Options[A] { self =>

    def ? : Options[Option[A]] = optional

    def ??(that: String): Single[A] = copy(description = description :+ that)

    def alias(name: String): Options[A] = copy(aliases = aliases :+ name)

    def aliases(names: String*): Options[A] = copy(aliases = aliases ++ names)

    def collect[B](message: String)(f: PartialFunction[A, B]): Options[B] =
      Map(self, (a: A) => f.lift(a).fold[Either[HelpDoc.Block, B]](Left(p(error(message))))(Right(_)))

    def optional: Options[Option[A]] = Optional(self)

    def recognizes(value: String, opts: ParserOptions): Option[Int] =
      if (supports(value, opts)) Some(1) else None

    def validate(args: List[String], opts: ParserOptions): IO[List[HelpDoc.Block], (List[String], A)] =
      args match {
        case head :: tail if supports(head, opts) =>
          optionType.validate(fullname, tail)
        case head :: tail =>
          validate(tail, opts).map {
            case (args, a) => (head :: args, a)
          }
        case Nil =>
          IO.fail(p(error(s"Expected to find ${fullname} option.")) :: Nil)
      }

    private[cli] def supports(arg: String, opts: ParserOptions) =
      opts.normalizeCase(arg) == fullname || aliases.map("-" + opts.normalizeCase(_)).contains(arg)

    private[cli] def fullname = "--" + name

  }

  final case class Optional[A](single: Single[A]) extends Options[Option[A]] {
    def recognizes(value: String, opts: ParserOptions): Option[Int] =
      single.recognizes(value, opts)

    def validate(args: List[String], opts: ParserOptions): IO[List[HelpDoc.Block], (List[String], Option[A])] =
      // single.validate(args, opts).map {
      //   case (args, a) => (args, Some(a))
      // } orElse ZIO.succeed((args, None))
      args match {
        case l @ head :: _ if single.supports(head, opts) =>
          single.validate(l, opts).map(r => r._1 -> Some(r._2))
        case head :: tail =>
          validate(tail, opts).map {
            case (args, a) => (head :: args, a)
          }
        case Nil => IO.succeed(args -> None)
      }
  }

  final case class Cons[A, B](left: Options[A], right: Options[B]) extends Options[(A, B)] {
    def recognizes(value: String, opts: ParserOptions): Option[Int] =
      left.recognizes(value, opts) orElse right.recognizes(value, opts)

    override def validate(args: List[String], opts: ParserOptions): IO[List[HelpDoc.Block], (List[String], (A, B))] =
      (for {
        tuple     <- left.validate(args, opts)
        (args, a) = tuple
        tuple     <- right.validate(args, opts)
        (args, b) = tuple
      } yield (args -> (a -> b))) orElse
        (for {
          tuple     <- right.validate(args, opts)
          (args, b) = tuple
          tuple     <- left.validate(args, opts)
          (args, a) = tuple
        } yield (args -> (a -> b)))
  }

  final case class Requires[A, B](options: Options[A], target: Options[B], predicate: B => Boolean) extends Options[A] {
    def recognizes(value: String, opts: ParserOptions): Option[Int] = options.recognizes(value, opts)

    def validate(args: List[String], opts: ParserOptions): IO[List[HelpDoc.Block], (List[String], A)] =
      target.validate(args, opts).foldM(f => IO.fail(f), _ => options.validate(args, opts))
  }

  final case class RequiresNot[A, B](options: Options[A], target: Options[B], predicate: B => Boolean)
      extends Options[A] {
    def recognizes(value: String, opts: ParserOptions): Option[Int] = options.recognizes(value, opts)

    def validate(args: List[String], opts: ParserOptions): IO[List[HelpDoc.Block], (List[String], A)] =
      target
        .validate(args, opts)
        .foldM(
          _ => options.validate(args, opts),
          _ => IO.fail(p(error("Requires not conditions were not satisfied.")) :: Nil)
        )
  }

  final case class Map[A, B](value: Options[A], f: A => Either[HelpDoc.Block, B]) extends Options[B] {
    def recognizes(v: String, opts: ParserOptions): Option[Int] = value.recognizes(v, opts)

    def validate(args: List[String], opts: ParserOptions): IO[List[HelpDoc.Block], (List[String], B)] =
      value.validate(args, opts).flatMap(r => f(r._2).fold(e => IO.fail(e :: Nil), s => IO.succeed(r._1 -> s)))
  }

  sealed trait Type[+A] {
    def validate(name: String, args: List[String]): IO[List[HelpDoc.Block], (List[String], A)]

  }
  object Type {
    final case class Toggle(negationName: Option[String], ifPresent: Boolean) extends Type[Boolean] {
      def validate(name: String, args: List[String]): IO[List[HelpDoc.Block], (List[String], Boolean)] = ???
    }

    final case class Primitive[A](primType: PrimType[A]) extends Type[A] {
      def validate(name: String, args: List[String]): IO[List[HelpDoc.Block], (List[String], A)] =
        args match {
          case head :: tail => primType.validate(head).bimap(f => p(f) :: Nil, a => tail -> a)
          case Nil          => IO.fail(p(error(s"Value for option ${name} was not found!")) :: Nil)
        }
    }
  }

  import Type._

  /**
   * Creates a boolean flag with the specified name, which, if present, will
   * produce the specified constant boolean value.
   */
  def bool(name: String, ifPresent: Boolean, negationName: Option[String] = None): Single[Boolean] =
    Single(name, Vector.empty, Type.Toggle(negationName, ifPresent), Vector.empty)

  def file(name: String, exists: Boolean): Single[JPath] =
    Single(name, Vector.empty, Primitive(PrimType.Path(PrimType.PathType.File, exists)), Vector.empty)

  def directory(name: String, exists: Boolean): Single[JPath] =
    Single(name, Vector.empty, Primitive(PrimType.Path(PrimType.PathType.Directory, exists)), Vector.empty)

  def text(name: String): Single[String] =
    Single(name, Vector.empty, Primitive(PrimType.Text), Vector.empty)

  def decimal(name: String): Single[BigDecimal] =
    Single(name, Vector.empty, Primitive(PrimType.Decimal), Vector.empty)

  def integer(name: String): Single[BigInt] =
    Single(name, Vector.empty, Primitive(PrimType.Integer), Vector.empty)

  def instant(name: String): Single[JInstant] =
    Single(name, Vector.empty, Primitive(PrimType.Instant), Vector.empty)

  def localDate(name: String): Single[JLocalDate] =
    Single(name, Vector.empty, Primitive(PrimType.LocalDate), Vector.empty)

  def localDateTime(name: String): Single[JLocalDateTime] =
    Single(name, Vector.empty, Primitive(PrimType.LocalDateTime), Vector.empty)

  def localTime(name: String): Single[JLocalTime] =
    Single(name, Vector.empty, Primitive(PrimType.LocalTime), Vector.empty)

  def monthDay(name: String): Single[JMonthDay] =
    Single(name, Vector.empty, Primitive(PrimType.MonthDay), Vector.empty)

  def offsetDateTime(name: String): Single[JOffsetDateTime] =
    Single(name, Vector.empty, Primitive(PrimType.OffsetDateTime), Vector.empty)

  def offsetTime(name: String): Single[JOffsetTime] =
    Single(name, Vector.empty, Primitive(PrimType.OffsetTime), Vector.empty)

  def period(name: String): Single[JPeriod] =
    Single(name, Vector.empty, Primitive(PrimType.Period), Vector.empty)

  def year(name: String): Single[JYear] =
    Single(name, Vector.empty, Primitive(PrimType.Year), Vector.empty)

  def yearMonth(name: String): Single[JYearMonth] =
    Single(name, Vector.empty, Primitive(PrimType.YearMonth), Vector.empty)

  def zonedDateTime(name: String): Single[JZonedDateTime] =
    Single(name, Vector.empty, Primitive(PrimType.ZonedDateTime), Vector.empty)

  def zoneId(name: String): Single[JZoneId] =
    Single(name, Vector.empty, Primitive(PrimType.ZoneId), Vector.empty)

  def zoneOffset(name: String): Single[JZoneOffset] =
    Single(name, Vector.empty, Primitive(PrimType.ZoneOffset), Vector.empty)

  val empty: Options[Unit] = Empty
}
