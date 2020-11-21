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
import zio.cli.HelpDoc.{ blocks, p }
import zio.cli.HelpDoc.Span._

import scala.collection.immutable.Nil

/**
 * A `Flag[A]` models a command-line flag that produces a value of type `A`.
 */
sealed trait Options[+A] { self =>
  import Options.Single

  final def ::[That, A1 >: A](that: Options[That]): Options[(That, A1)] =
    Options.Cons(that, self)

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

  final def collect[B](message: String)(f: PartialFunction[A, B]): Options[B] =
    Options.Map(self, (a: A) => f.lift(a).fold[Either[HelpDoc, B]](Left(p(error(message))))(Right(_)))

  final def flatten2[B, C](implicit ev: A <:< ((B, C))): Options[(B, C)] = as[B, C, (B, C)]((_, _))

  final def flatten3[B, C, D](implicit ev: A <:< ((B, (C, D)))): Options[(B, C, D)] = as[B, C, D, (B, C, D)]((_, _, _))

  final def flatten4[B, C, D, E](implicit ev: A <:< ((B, (C, (D, E))))): Options[(B, C, D, E)] =
    as[B, C, D, E, (B, C, D, E)]((_, _, _, _))

  final def flatten5[B, C, D, E, F](implicit ev: A <:< ((B, (C, (D, (E, F)))))): Options[(B, C, D, E, F)] =
    as[B, C, D, E, F, (B, C, D, E, F)]((_, _, _, _, _))

  final def flatten6[B, C, D, E, F, G](implicit ev: A <:< ((B, (C, (D, (E, (F, G))))))): Options[(B, C, D, E, F, G)] =
    as[B, C, D, E, F, G, (B, C, D, E, F, G)]((_, _, _, _, _, _))

  def helpDoc: HelpDoc

  final def map[B](f: A => B): Options[B] = Options.Map(self, (a: A) => Right(f(a)))

  final def mapOrFail[B](f: A => Either[HelpDoc, B]): Options[B] =
    Options.Map(self, (a: A) => f(a))

  final def mapTry[B](f: A => B): Options[B] =
    self.mapOrFail((a: A) => scala.util.Try(f(a)).toEither.left.map(e => p(e.getMessage())))

  final def optional(desc: String): Options[Option[A]] = self.map(Some(_)).withDefault(None, desc)

  def recognizes(value: String, opts: ParserOptions): Option[Int]

  final def requires[B](that: Options[B], suchThat: B => Boolean = (_: B) => true): Options[A] =
    Options.Requires(self, that, suchThat)

  final def requiresNot[B](that: Options[B], suchThat: B => Boolean = (_: B) => true): Options[A] =
    Options.RequiresNot(self, that, suchThat)

  def synopsis: UsageSynopsis

  def uid: Option[String]

  def validate(args: List[String], opts: ParserOptions): IO[HelpDoc, (List[String], A)]

  def withDefault[A1 >: A](value: A1, valueDescription: String): Options[A1] =
    Options.WithDefault(self, value, valueDescription)

  private[cli] def modifySingle(f: SingleModifier): Options[A]

  private[cli] def foldSingle[C](initial: C)(f: (C, Single[_]) => C): C = self match {
    case _: Options.Empty.type                   => initial
    case s @ Single(_, _, _, _)                  => f(initial, s)
    case cons: Options.Cons[a, b]                => cons.right.foldSingle(cons.left.foldSingle(initial)(f))(f)
    case Options.Requires(options, target, _)    => options.foldSingle(initial)(f)
    case Options.RequiresNot(options, target, _) => options.foldSingle(initial)(f)
    case Options.Map(value, _)                   => value.foldSingle(initial)(f)
    case Options.WithDefault(value, _, _)        => value.foldSingle(initial)(f)
  }

  private[cli] def supports(arg: String, opts: ParserOptions) =
    foldSingle(false) {
      case (bool, single) =>
        bool || Some(opts.normalizeCase(arg)) == single.uid || single.aliases
          .map("-" + opts.normalizeCase(_))
          .contains(arg)
    }
}

trait SingleModifier {
  def apply[A](single: Options.Single[A]): Options.Single[A]
}

object Options {
  case object Empty extends Options[Unit] {
    def recognizes(value: String, opts: ParserOptions): Option[Int] = None

    def synopsis: UsageSynopsis = UsageSynopsis.None

    def validate(args: List[String], opts: ParserOptions): IO[HelpDoc, (List[String], Unit)] =
      IO.succeed((args, ()))

    override def modifySingle(f: SingleModifier): Options[Unit] = Empty

    override def helpDoc: HelpDoc = HelpDoc.Empty

    override def uid: Option[String] = None
  }

  final case class WithDefault[A](options: Options[A], default: A, defaultDescription: String) extends Options[A] {
    def recognizes(value: String, opts: ParserOptions): Option[Int] = options.recognizes(value, opts)

    def synopsis: UsageSynopsis = options.synopsis

    def validate(args: List[String], opts: ParserOptions): IO[HelpDoc, (List[String], A)] =
      options.validate(args, opts) orElseSucceed (args -> default)

    override def modifySingle(f: SingleModifier): Options[A] =
      WithDefault(options.modifySingle(f), default, defaultDescription)

    override def helpDoc: HelpDoc =
      options.helpDoc.mapDescriptionList {
        case (span, block) =>
          span -> (block + HelpDoc.p(
            s"This setting is optional. If unspecified, the default value of this option is ${defaultDescription}."
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

    def recognizes(value: String, opts: ParserOptions): Option[Int] =
      if (supports(value, opts)) Some(1) else None

    def synopsis: UsageSynopsis =
      UsageSynopsis.Named(fullname, primType.choices)

    def validate(args: List[String], opts: ParserOptions): IO[HelpDoc, (List[String], A)] =
      args match {
        case head :: tail if supports(head, opts) =>
          (tail match {
            case Nil         => primType.validate(None, opts)
            case ::(head, _) => primType.validate(Some(head), opts)
          }).bimap(f => p(f), a => tail.drop(1) -> a)

        case head :: tail =>
          validate(tail, opts).map {
            case (args, a) => (head :: args, a)
          }
        case Nil =>
          IO.fail(p(error(s"Expected to find ${fullname} option.")))
      }

    def uid = Some(fullname)

    private def fullname: String = (if (name.length == 1) "-" else "--") + name

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

  final case class Cons[A, B](left: Options[A], right: Options[B]) extends Options[(A, B)] {
    override def modifySingle(f: SingleModifier): Options[(A, B)] = Cons(left.modifySingle(f), right.modifySingle(f))

    def recognizes(value: String, opts: ParserOptions): Option[Int] =
      left.recognizes(value, opts) orElse right.recognizes(value, opts)

    def synopsis: UsageSynopsis = left.synopsis + right.synopsis

    override def validate(args: List[String], opts: ParserOptions): IO[HelpDoc, (List[String], (A, B))] =
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

    override def helpDoc: HelpDoc = left.helpDoc + right.helpDoc

    override def uid: Option[String] = (left.uid.toList ++ right.uid.toList) match {
      case Nil  => None
      case list => Some(list.mkString(", "))
    }
  }

  final case class Requires[A, B](options: Options[A], target: Options[B], predicate: B => Boolean) extends Options[A] {
    override def modifySingle(f: SingleModifier): Options[A] =
      Requires(options.modifySingle(f), target.modifySingle(f), predicate)

    def recognizes(value: String, opts: ParserOptions): Option[Int] = options.recognizes(value, opts)

    def synopsis: UsageSynopsis = options.synopsis

    def validate(args: List[String], opts: ParserOptions): IO[HelpDoc, (List[String], A)] =
      target.validate(args, opts).foldM(f => IO.fail(f), _ => options.validate(args, opts))

    override def helpDoc: HelpDoc = options.helpDoc.mapDescriptionList {
      case (span, block) =>
        target.uid match {
          case Some(value) => (span, blocks(block, p(s"This option must be used in combination with ${value}.")))
          case None        => (span, block)
        }
    }

    override def uid: Option[String] = options.uid
  }

  final case class RequiresNot[A, B](options: Options[A], target: Options[B], predicate: B => Boolean)
      extends Options[A] {
    override def modifySingle(f: SingleModifier): Options[A] =
      RequiresNot(options.modifySingle(f), target.modifySingle(f), predicate)

    def recognizes(value: String, opts: ParserOptions): Option[Int] = options.recognizes(value, opts)

    def synopsis: UsageSynopsis = options.synopsis

    def validate(args: List[String], opts: ParserOptions): IO[HelpDoc, (List[String], A)] =
      target
        .validate(args, opts)
        .foldM(
          _ => options.validate(args, opts),
          _ => IO.fail(p(error("Requires not conditions were not satisfied.")))
        )

    override def helpDoc: HelpDoc = options.helpDoc.mapDescriptionList { (span, block) =>
      target.uid match {
        case Some(value) => (span, blocks(block, p(s"This option may not be used in combination with ${value}.")))
        case None        => (span, block)
      }
    }

    override def uid: Option[String] = options.uid
  }

  final case class Map[A, B](value: Options[A], f: A => Either[HelpDoc, B]) extends Options[B] {
    override def modifySingle(f0: SingleModifier): Options[B] = Map(value.modifySingle(f0), f)

    def recognizes(v: String, opts: ParserOptions): Option[Int] = value.recognizes(v, opts)

    def synopsis: UsageSynopsis = value.synopsis

    def validate(args: List[String], opts: ParserOptions): IO[HelpDoc, (List[String], B)] =
      value.validate(args, opts).flatMap(r => f(r._2).fold(e => IO.fail(e), s => IO.succeed(r._1 -> s)))

    override def uid: Option[String] = value.uid

    override def helpDoc: HelpDoc = value.helpDoc
  }

  /**
   * Creates a boolean flag with the specified name, which, if present, will
   * produce the specified constant boolean value.
   */
  def bool(name: String, ifPresent: Boolean, negationName: Option[String] = None): Options[Boolean] = {
    // TODO
    val _ = negationName
    Single(name, Vector.empty, PrimType.Bool(Some(ifPresent)))
      .withDefault(!ifPresent, (!ifPresent).toString)
  }

  def enumeration[A](name: String)(cases: (String, A)*): Options[A] =
    Single(name, Vector.empty, PrimType.Enumeration(cases: _*))

  def file(name: String, exists: Exists = Exists.Either): Options[JPath] =
    Single(name, Vector.empty, PrimType.Path(PathType.File, exists))

  def directory(name: String, exists: Exists = Exists.Either): Options[JPath] =
    Single(name, Vector.empty, PrimType.Path(PathType.Directory, exists))

  def text(name: String): Options[String] =
    Single(name, Vector.empty, PrimType.Text)

  def decimal(name: String): Options[BigDecimal] =
    Single(name, Vector.empty, PrimType.Decimal)

  def integer(name: String): Options[BigInt] =
    Single(name, Vector.empty, PrimType.Integer)

  def instant(name: String): Options[JInstant] =
    Single(name, Vector.empty, PrimType.Instant)

  def localDate(name: String): Options[JLocalDate] =
    Single(name, Vector.empty, PrimType.LocalDate)

  def localDateTime(name: String): Options[JLocalDateTime] =
    Single(name, Vector.empty, PrimType.LocalDateTime)

  def localTime(name: String): Options[JLocalTime] =
    Single(name, Vector.empty, PrimType.LocalTime)

  def monthDay(name: String): Options[JMonthDay] =
    Single(name, Vector.empty, PrimType.MonthDay)

  val none: Options[Unit] = Empty

  def offsetDateTime(name: String): Options[JOffsetDateTime] =
    Single(name, Vector.empty, PrimType.OffsetDateTime)

  def offsetTime(name: String): Options[JOffsetTime] =
    Single(name, Vector.empty, PrimType.OffsetTime)

  def period(name: String): Options[JPeriod] =
    Single(name, Vector.empty, PrimType.Period)

  def year(name: String): Options[JYear] =
    Single(name, Vector.empty, PrimType.Year)

  def yearMonth(name: String): Options[JYearMonth] =
    Single(name, Vector.empty, PrimType.YearMonth)

  def zonedDateTime(name: String): Options[JZonedDateTime] =
    Single(name, Vector.empty, PrimType.ZonedDateTime)

  def zoneId(name: String): Options[JZoneId] =
    Single(name, Vector.empty, PrimType.ZoneId)

  def zoneOffset(name: String): Options[JZoneOffset] =
    Single(name, Vector.empty, PrimType.ZoneOffset)
}
