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
import zio.cli.Options.{ Map, Optional, Single }

import scala.collection.immutable.Nil

/**
 * A `Flag[A]` models a command-line flag that produces a value of type `A`.
 */
sealed trait Options[+A] { self =>

  final def ::[That, A1 >: A](that: Options[That]): Options[(That, A1)] =
    Options.Cons(that, self)

  def ? : Options[Option[A]] = optional

  def ??(that: String): Options[A] =
    modifySingle(new SingleModifier {
      override def apply[A](single: Single[A]): Single[A] = single.copy(description = single.description :+ that)
    })

  def alias(name: String): Options[A] =
    modifySingle(new SingleModifier {
      override def apply[A](single: Single[A]): Single[A] = single.copy(aliases = single.aliases :+ name)
    })

  def aliases(names: String*): Options[A] =
    modifySingle(new SingleModifier {
      override def apply[A](single: Single[A]): Single[A] = single.copy(aliases = single.aliases ++ names)
    })

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

  def collect[B](message: String)(f: PartialFunction[A, B]): Options[B] =
    Map(self, (a: A) => f.lift(a).fold[Either[HelpDoc, B]](Left(p(error(message))))(Right(_)))

  final def flatten2[B, C](implicit ev: A <:< ((B, C))): Options[(B, C)] = as[B, C, (B, C)]((_, _))

  final def flatten3[B, C, D](implicit ev: A <:< ((B, (C, D)))): Options[(B, C, D)] = as[B, C, D, (B, C, D)]((_, _, _))

  final def flatten4[B, C, D, E](implicit ev: A <:< ((B, (C, (D, E))))): Options[(B, C, D, E)] =
    as[B, C, D, E, (B, C, D, E)]((_, _, _, _))

  final def flatten5[B, C, D, E, F](implicit ev: A <:< ((B, (C, (D, (E, F)))))): Options[(B, C, D, E, F)] =
    as[B, C, D, E, F, (B, C, D, E, F)]((_, _, _, _, _))

  final def flatten6[B, C, D, E, F, G](implicit ev: A <:< ((B, (C, (D, (E, (F, G))))))): Options[(B, C, D, E, F, G)] =
    as[B, C, D, E, F, G, (B, C, D, E, F, G)]((_, _, _, _, _, _))

  def uid: Option[String]

  private[cli] def foldSingle[C](initial: C)(f: (C, Single[_]) => C): C = self match {
    case _: Options.Empty.type                   => initial
    case s @ Single(_, _, _, _)                  => f(initial, s)
    case opt: Optional[a]                        => opt.options.foldSingle(initial)(f)
    case cons: Options.Cons[a, b]                => cons.right.foldSingle(cons.left.foldSingle(initial)(f))(f)
    case Options.Requires(options, target, _)    => target.foldSingle(options.foldSingle(initial)(f))(f)
    case Options.RequiresNot(options, target, _) => target.foldSingle(options.foldSingle(initial)(f))(f)
    case Map(value, _)                           => value.foldSingle(initial)(f)
  }

  def helpDoc: List[(HelpDoc.Span, HelpDoc)]

  final def map[B](f: A => B): Options[B] = Options.Map(self, (a: A) => Right(f(a)))

  final def mapTry[B](f: A => B): Options[B] =
    Options.Map(self, (a: A) => scala.util.Try(f(a)).toEither.left.map(e => p(error(e.getMessage))))

  private[cli] def modifySingle(f: SingleModifier): Options[A]

  def optional: Options[Option[A]] = Optional(self)

  def recognizes(value: String, opts: ParserOptions): Option[Int]

  final def requires[B](that: Options[B], suchThat: B => Boolean = (_: B) => true): Options[A] =
    Options.Requires(self, that, suchThat)

  final def requiresNot[B](that: Options[B], suchThat: B => Boolean = (_: B) => true): Options[A] =
    Options.RequiresNot(self, that, suchThat)

  private[cli] def supports(arg: String, opts: ParserOptions) =
    foldSingle(false) {
      case (bool, single) =>
        bool || opts.normalizeCase(arg) == single.uid || single.aliases
          .map("-" + opts.normalizeCase(_))
          .contains(arg)
    }

  def validate(args: List[String], opts: ParserOptions): IO[List[HelpDoc], (List[String], A)]
}

trait SingleModifier {
  def apply[A](single: Single[A]): Single[A]
}

object Options {
  // --verbose 3
  case object Empty extends Options[Unit] {
    def recognizes(value: String, opts: ParserOptions): Option[Int] = None

    def validate(args: List[String], opts: ParserOptions): IO[List[HelpDoc], (List[String], Unit)] =
      IO.succeed((args, ()))

    override def modifySingle(f: SingleModifier): Options[Unit] = Empty

    //TODO
    override def helpDoc: List[(HelpDoc.Span, HelpDoc)] = List.empty

    override def uid: Option[String] = None
  }

  final case class Single[+A](
    name: String,
    aliases: Vector[String],
    optionType: Options.Type[A],
    description: Vector[String]
  ) extends Options[A] { self =>

    override def modifySingle(f: SingleModifier): Options[A] = f(self)

    def recognizes(value: String, opts: ParserOptions): Option[Int] =
      if (supports(value, opts)) Some(1) else None

    def validate(args: List[String], opts: ParserOptions): IO[List[HelpDoc], (List[String], A)] =
      args match {
        case head :: tail if supports(head, opts) =>
          optionType.validate(fName, tail)
        case head :: tail =>
          validate(tail, opts).map {
            case (args, a) => (head :: args, a)
          }
        case Nil =>
          IO.fail(p(error(s"Expected to find ${fName} option.")) :: Nil)
      }

    def uid = Some(fName)

    private def fName: String = "--" + name

    override def helpDoc: List[(HelpDoc.Span, HelpDoc)] = {

      val allNames = Vector("--" + name) ++ aliases.map("--" + _)

      List(
        spans(allNames.map(weak(_)).zipWithIndex.map {
          case (span, index) => if (index != allNames.length - 1) span + Span.text(", ") else span
        }) ->
          blocks(optionType.helpDoc, blocks(description.map(p(_))))
      )
    }
  }

  final case class Optional[A](options: Options[A]) extends Options[Option[A]] { self =>
    override def modifySingle(f: SingleModifier): Options[Option[A]] = Optional(options.modifySingle(f))

    def recognizes(value: String, opts: ParserOptions): Option[Int] =
      options.recognizes(value, opts)

    def validate(args: List[String], opts: ParserOptions): IO[List[HelpDoc], (List[String], Option[A])] =
      // single.validate(args, opts).map {
      //   case (args, a) => (args, Some(a))
      // } orElse ZIO.succeed((args, None))
      args match {
        case l @ head :: _ if options.supports(head, opts) =>
          options.validate(l, opts).map(r => r._1 -> Some(r._2))
        case head :: tail =>
          validate(tail, opts).map {
            case (args, a) => (head :: args, a)
          }
        case Nil => IO.succeed(args -> None)
      }

    override def helpDoc: List[(HelpDoc.Span, HelpDoc)] = options.helpDoc.map {
      case (span, block) => (span, blocks(block, p("This option is optional.")))
    }

    override def uid: Option[String] = options.uid
  }

  final case class Cons[A, B](left: Options[A], right: Options[B]) extends Options[(A, B)] {
    override def modifySingle(f: SingleModifier): Options[(A, B)] = Cons(left.modifySingle(f), right.modifySingle(f))

    def recognizes(value: String, opts: ParserOptions): Option[Int] =
      left.recognizes(value, opts) orElse right.recognizes(value, opts)

    override def validate(args: List[String], opts: ParserOptions): IO[List[HelpDoc], (List[String], (A, B))] =
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

    override def helpDoc: List[(HelpDoc.Span, HelpDoc)] = left.helpDoc ++ right.helpDoc

    override def uid: Option[String] = (left.uid.toList ++ right.uid.toList) match {
      case Nil  => None
      case list => Some(list.mkString(", "))
    }
  }

  final case class Requires[A, B](options: Options[A], target: Options[B], predicate: B => Boolean) extends Options[A] {
    override def modifySingle(f: SingleModifier): Options[A] =
      Requires(options.modifySingle(f), target.modifySingle(f), predicate)

    def recognizes(value: String, opts: ParserOptions): Option[Int] = options.recognizes(value, opts)

    def validate(args: List[String], opts: ParserOptions): IO[List[HelpDoc], (List[String], A)] =
      target.validate(args, opts).foldM(f => IO.fail(f), _ => options.validate(args, opts))

    override def helpDoc: List[(HelpDoc.Span, HelpDoc)] = options.helpDoc.map {
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

    def validate(args: List[String], opts: ParserOptions): IO[List[HelpDoc], (List[String], A)] =
      target
        .validate(args, opts)
        .foldM(
          _ => options.validate(args, opts),
          _ => IO.fail(p(error("Requires not conditions were not satisfied.")) :: Nil)
        )

    override def helpDoc: List[(HelpDoc.Span, HelpDoc)] = options.helpDoc.map {
      case (span, block) =>
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

    def validate(args: List[String], opts: ParserOptions): IO[List[HelpDoc], (List[String], B)] =
      value.validate(args, opts).flatMap(r => f(r._2).fold(e => IO.fail(e :: Nil), s => IO.succeed(r._1 -> s)))

    override def uid: Option[String] = value.uid

    override def helpDoc: List[(HelpDoc.Span, HelpDoc)] = value.helpDoc
  }

  sealed trait Type[+A] {
    def validate(name: String, args: List[String]): IO[List[HelpDoc], (List[String], A)]
    def helpDoc: HelpDoc

  }
  object Type {
    final case class Toggle(negationName: Option[String], ifPresent: Boolean) extends Type[Boolean] {
      def validate(name: String, args: List[String]): IO[List[HelpDoc], (List[String], Boolean)] =
        IO.effectTotal((args, ifPresent))

      override def helpDoc: HelpDoc =
        p(s"A boolean toggle. If present, this option will be ${ifPresent}." + (negationName match {
          case None                     => ""
          case Some(value) if ifPresent => s" To turn this toggle off, use --${value}."
          case Some(value)              => s" To turn this toggle on, use --${value}."
        }))
    }

    final case class Primitive[A](primType: PrimType[A]) extends Type[A] {
      def validate(name: String, args: List[String]): IO[List[HelpDoc], (List[String], A)] =
        args match {
          case head :: tail => primType.validate(head).bimap(f => p(f) :: Nil, a => tail -> a)
          case Nil          => IO.fail(p(error(s"Value for option ${name} was not found!")) :: Nil)
        }

      override def helpDoc: HelpDoc = p(primType.helpDoc)
    }
  }

  import Type._

  /**
   * Creates a boolean flag with the specified name, which, if present, will
   * produce the specified constant boolean value.
   */
  def bool(name: String, ifPresent: Boolean, negationName: Option[String] = None): Options[Boolean] =
    Single(name, Vector.empty, Type.Toggle(negationName, ifPresent), Vector.empty).optional.map(_.getOrElse(!ifPresent))

  def file(name: String, exists: Boolean): Options[JPath] =
    Single(name, Vector.empty, Primitive(PrimType.Path(PrimType.PathType.File, exists)), Vector.empty)

  def directory(name: String, exists: Boolean): Options[JPath] =
    Single(name, Vector.empty, Primitive(PrimType.Path(PrimType.PathType.Directory, exists)), Vector.empty)

  def text(name: String): Options[String] =
    Single(name, Vector.empty, Primitive(PrimType.Text), Vector.empty)

  def decimal(name: String): Options[BigDecimal] =
    Single(name, Vector.empty, Primitive(PrimType.Decimal), Vector.empty)

  def integer(name: String): Options[BigInt] =
    Single(name, Vector.empty, Primitive(PrimType.Integer), Vector.empty)

  def instant(name: String): Options[JInstant] =
    Single(name, Vector.empty, Primitive(PrimType.Instant), Vector.empty)

  def localDate(name: String): Options[JLocalDate] =
    Single(name, Vector.empty, Primitive(PrimType.LocalDate), Vector.empty)

  def localDateTime(name: String): Options[JLocalDateTime] =
    Single(name, Vector.empty, Primitive(PrimType.LocalDateTime), Vector.empty)

  def localTime(name: String): Options[JLocalTime] =
    Single(name, Vector.empty, Primitive(PrimType.LocalTime), Vector.empty)

  def monthDay(name: String): Options[JMonthDay] =
    Single(name, Vector.empty, Primitive(PrimType.MonthDay), Vector.empty)

  def offsetDateTime(name: String): Options[JOffsetDateTime] =
    Single(name, Vector.empty, Primitive(PrimType.OffsetDateTime), Vector.empty)

  def offsetTime(name: String): Options[JOffsetTime] =
    Single(name, Vector.empty, Primitive(PrimType.OffsetTime), Vector.empty)

  def period(name: String): Options[JPeriod] =
    Single(name, Vector.empty, Primitive(PrimType.Period), Vector.empty)

  def year(name: String): Options[JYear] =
    Single(name, Vector.empty, Primitive(PrimType.Year), Vector.empty)

  def yearMonth(name: String): Options[JYearMonth] =
    Single(name, Vector.empty, Primitive(PrimType.YearMonth), Vector.empty)

  def zonedDateTime(name: String): Options[JZonedDateTime] =
    Single(name, Vector.empty, Primitive(PrimType.ZonedDateTime), Vector.empty)

  def zoneId(name: String): Options[JZoneId] =
    Single(name, Vector.empty, Primitive(PrimType.ZoneId), Vector.empty)

  def zoneOffset(name: String): Options[JZoneOffset] =
    Single(name, Vector.empty, Primitive(PrimType.ZoneOffset), Vector.empty)

  val empty: Options[Unit] = Empty
}
