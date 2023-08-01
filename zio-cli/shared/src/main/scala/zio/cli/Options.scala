package zio.cli

import zio.cli.HelpDoc.Span._
import zio.cli.HelpDoc.p
import zio.{IO, ZIO, Zippable}
import zio.cli.oauth2._

import java.nio.file.{Path => JPath}
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
import scala.annotation.tailrec

/**
 * A `Flag[A]` models a command-line flag that produces a value of type `A`.
 */
sealed trait Options[+A] extends Parameter {
  self =>

  import Options.Single

  final def ++[A1 >: A, B](that: Options[B])(implicit zippable: Zippable[A, B]): Options[zippable.Out] =
    Options.Both(self, that).map { case (a, b) => zippable.zip(a, b) }

  final def |[A1 >: A](that: Options[A1]): Options[A1] = orElse(that)

  final def orElse[A1 >: A](that: Options[A1]): Options[A1] =
    orElseEither(that).map(_.merge)

  final def orElseEither[B](that: Options[B]): Options[Either[A, B]] =
    Options.OrElse(self, that)

  def ??(that: String): Options[A] =
    modifySingle(new SingleModifier {
      override def apply[A](single: Single[A]): Single[A] = single.copy(description = single.description + p(that))
    })

  def alias(name: String, names: String*): Options[A] =
    modifySingle(new SingleModifier {
      override def apply[A](single: Single[A]): Single[A] = single.copy(aliases = single.aliases ++ (name +: names))
    })

  final def as[B, C, Z](f: (B, C) => Z)(implicit ev: A <:< (B, C)): Options[Z] =
    map(ev).map { case (b, c) => f(b, c) }

  final def as[B, C, D, Z](f: (B, C, D) => Z)(implicit ev: A <:< (B, C, D)): Options[Z] =
    map(ev).map { case (b, c, d) => f(b, c, d) }

  final def as[B, C, D, E, Z](f: (B, C, D, E) => Z)(implicit ev: A <:< (B, C, D, E)): Options[Z] =
    map(ev).map { case (b, c, d, e) => f(b, c, d, e) }

  final def as[B, C, D, E, F, Z](f0: (B, C, D, E, F) => Z)(implicit ev: A <:< (B, C, D, E, F)): Options[Z] =
    map(ev).map { case (b, c, d, e, f) => f0(b, c, d, e, f) }

  final def as[B, C, D, E, F, G, Z](
    f0: (B, C, D, E, F, G) => Z
  )(implicit ev: A <:< (B, C, D, E, F, G)): Options[Z] =
    map(ev).map { case (b, c, d, e, f, g) => f0(b, c, d, e, f, g) }

  final def fold[B, C, Z](
    f1: B => Z,
    f2: C => Z
  )(implicit ev: A <:< Either[B, C]): Options[Z] =
    map(ev).map {
      case Left(b)  => f1(b)
      case Right(c) => f2(c)
    }

  final def fold[B, C, D, Z](
    f1: B => Z,
    f2: C => Z,
    f3: D => Z
  )(implicit ev: A <:< Either[Either[B, C], D]): Options[Z] =
    map(ev).map {
      case Left(Left(b))  => f1(b)
      case Left(Right(c)) => f2(c)
      case Right(d)       => f3(d)
    }

  final def fold[B, C, D, E, Z](
    f1: B => Z,
    f2: C => Z,
    f3: D => Z,
    f4: E => Z
  )(implicit ev: A <:< Either[Either[Either[B, C], D], E]): Options[Z] =
    map(ev).map {
      case Left(Left(Left(b)))  => f1(b)
      case Left(Left(Right(c))) => f2(c)
      case Left(Right(d))       => f3(d)
      case Right(e)             => f4(e)
    }

  final def fold[B, C, D, E, F, Z](
    f1: B => Z,
    f2: C => Z,
    f3: D => Z,
    f4: E => Z,
    f5: F => Z
  )(implicit ev: A <:< Either[Either[Either[Either[B, C], D], E], F]): Options[Z] =
    map(ev).map {
      case Left(Left(Left(Left(b))))  => f1(b)
      case Left(Left(Left(Right(c)))) => f2(c)
      case Left(Left(Right(d)))       => f3(d)
      case Left(Right(e))             => f4(e)
      case Right(f)                   => f5(f)
    }

  final def fold[B, C, D, E, F, G, Z](
    f1: B => Z,
    f2: C => Z,
    f3: D => Z,
    f4: E => Z,
    f5: F => Z,
    f6: G => Z
  )(implicit ev: A <:< Either[Either[Either[Either[Either[B, C], D], E], F], G]): Options[Z] =
    map(ev).map {
      case Left(Left(Left(Left(Left(b)))))  => f1(b)
      case Left(Left(Left(Left(Right(c))))) => f2(c)
      case Left(Left(Left(Right(d))))       => f3(d)
      case Left(Left(Right(e)))             => f4(e)
      case Left(Right(f))                   => f5(f)
      case Right(g)                         => f6(g)
    }

  final def collect[B](message: String)(f: PartialFunction[A, B]): Options[B] =
    Options.Map(
      self,
      (a: A) =>
        f.lift(a)
          .fold[Either[ValidationError, B]](Left(ValidationError(ValidationErrorType.InvalidValue, p(error(message)))))(
            Right(_)
          )
    )

  def helpDoc: HelpDoc

  final def isBool: Boolean =
    asInstanceOf[Options[_]] match {
      case Options.Empty                   => false
      case Options.WithDefault(options, _) => options.isBool
      case Single(_, _, primType, _, _)    => primType.isBool
      case Options.Map(value, _)           => value.isBool
      case _                               => false
    }

  final def map[B](f: A => B): Options[B] = Options.Map(self, (a: A) => Right(f(a)))

  final def mapOrFail[B](f: A => Either[ValidationError, B]): Options[B] =
    Options.Map(self, (a: A) => f(a))

  final def optional: Options[Option[A]] = map(Some(_)).withDefault(None)

  def synopsis: UsageSynopsis

  def uid: Option[String]

  def validate(args: List[String], conf: CliConfig): IO[ValidationError, (List[String], A)]

  def withDefault[A1 >: A](value: A1): Options[A1] =
    Options.WithDefault(self, value)

  /**
   * Customizes the name used to print a placeholder value in help strings.
   *
   * The default is the type name of the option (for example 'text', 'integer', etc.
   */
  def withPseudoName(name: String): Options[A] =
    modifySingle(new SingleModifier {
      override def apply[A2](single: Single[A2]): Single[A2] =
        single.copy(pseudoName = Some(name))
    })

  private[cli] def modifySingle(f: SingleModifier): Options[A]

  lazy val tag = "option"
}

trait SingleModifier {
  def apply[A](single: Options.Single[A]): Options.Single[A]
}

object Options extends OptionsPlatformSpecific {
  case object Empty extends Options[Unit] with Pipeline {
    lazy val synopsis: UsageSynopsis = UsageSynopsis.None

    def validate(args: List[String], conf: CliConfig): IO[ValidationError, (List[String], Unit)] =
      ZIO.succeed((args, ()))

    override def modifySingle(f: SingleModifier): Options[Unit] = Empty

    override lazy val helpDoc: HelpDoc = HelpDoc.Empty

    override lazy val uid: Option[String] = None

    override def pipeline = ("", List())
  }

  final case class WithDefault[A](options: Options[A], default: A) extends Options[A] with Input {

    override lazy val shortDesc: String = options.shortDesc

    lazy val synopsis: UsageSynopsis = options.synopsis.optional

    def validate(args: List[String], conf: CliConfig): IO[ValidationError, (List[String], A)] =
      options
        .validate(args, conf)
        .catchSome {
          case error if error.isOptionMissing => ZIO.succeed(args -> default)
        }

    override def modifySingle(f: SingleModifier): Options[A] =
      WithDefault(options.modifySingle(f), default)

    override lazy val helpDoc: HelpDoc =
      options.helpDoc.mapDescriptionList { case (span, block) =>
        val optionalDescription =
          default.asInstanceOf[Any] match {
            case None =>
              HelpDoc.p(s"This setting is optional.")
            case _ =>
              HelpDoc.p(s"This setting is optional. Default: '${default}'.")
          }
        span -> (block + optionalDescription)
      }

    override lazy val uid: Option[String] = options.uid

    override def isValid(input: String, conf: CliConfig): IO[ValidationError, List[String]] =
      ZIO.succeed(
        if (options.isBool) {
          if (PrimType.Bool.TrueValues.contains(input)) List(options.uid.getOrElse("")) else List.empty
        } else {
          if (input.isEmpty) List.empty else List(options.uid.getOrElse(""), input)
        }
      )
  }

  final case class Single[+A](
    name: String,
    aliases: Vector[String],
    primType: PrimType[A],
    description: HelpDoc = HelpDoc.Empty,
    pseudoName: Option[String] = None
  ) extends Options[A]
      with Input {
    self =>

    override lazy val shortDesc: String = s"""Option "$name". ${description.getSpan.text}"""

    override def modifySingle(f: SingleModifier): Options[A] = f(self)

    lazy val synopsis: UsageSynopsis =
      UsageSynopsis.Named(
        names,
        if (!primType.isBool) primType.choices.orElse(Some(placeholder)) else None
      )

    def validate(args: List[String], conf: CliConfig): IO[ValidationError, (List[String], A)] =
      args match {
        case head :: tail =>
          val (rest, supported) = processArg(head, tail, conf)
          if (supported) {
            primType match {
              case _: PrimType.Bool =>
                primType
                  .validate(None, conf)
                  .mapBoth(e => ValidationError(ValidationErrorType.InvalidValue, p(e)), rest -> _)
              case _ =>
                primType
                  .validate(rest.headOption, conf)
                  .mapBoth(e => ValidationError(ValidationErrorType.InvalidValue, p(e)), rest.drop(1) -> _)
            }
          } else if (
            name.length > conf.autoCorrectLimit + 1 && AutoCorrect.levensteinDistance(
              head,
              fullName,
              conf
            ) <= conf.autoCorrectLimit
          ) {
            ZIO.fail(
              ValidationError(
                ValidationErrorType.InvalidValue,
                p(error(s"""The flag "$head" is not recognized. Did you mean ${fullName}?"""))
              )
            )
          } else {
            validate(rest, conf).map { case (args, a) =>
              (head :: args, a)
            }
          }
        case Nil =>
          ZIO.fail(
            ValidationError(ValidationErrorType.MissingValue, p(error(s"Expected to find ${fullName} option.")))
          )
      }

    private def processArg(arg: String, args: List[String], conf: CliConfig): (List[String], Boolean) = {
      def process(predicate: String => Boolean): (List[String], Boolean) =
        if (predicate(arg)) (args, true)
        else if (arg.startsWith("--")) {
          val splitArg = arg.split("=", -1)
          if (splitArg.length == 2) (splitArg(1) :: args, predicate(splitArg.head))
          else (args, false)
        } else (args, false)

      if (conf.caseSensitive) process(names.contains)
      else process(s => names.exists(_.equalsIgnoreCase(s)))
    }

    lazy val uid: Option[String] = Some(fullName)

    private def makeFullName(s: String): (Boolean, String) = if (s.length == 1) (true, "-" + s) else (false, "--" + s)

    lazy val names: List[String] = (name :: aliases.toList).map(makeFullName).sortBy(!_._1).map(_._2)

    private lazy val fullName: String = makeFullName(name)._2

    override lazy val helpDoc: HelpDoc =
      HelpDoc.DescriptionList(List(synopsis.helpDoc.getSpan -> (p(primType.helpDoc) + description)))

    override def isValid(input: String, conf: CliConfig): IO[ValidationError, List[String]] = for {
      _ <- validate(List(names.head, input), conf)
    } yield List(names.head, input)

    private def placeholder: String = "<" + pseudoName.getOrElse(primType.typeName) + ">"
  }

  final case class OrElse[A, B](left: Options[A], right: Options[B]) extends Options[Either[A, B]] with Alternatives {

    override def modifySingle(f: SingleModifier): Options[Either[A, B]] =
      OrElse(left.modifySingle(f), right.modifySingle(f))

    lazy val synopsis: UsageSynopsis = UsageSynopsis.Alternation(left.synopsis, right.synopsis)

    override def validate(args: List[String], conf: CliConfig): IO[ValidationError, (List[String], Either[A, B])] =
      left
        .validate(args, conf)
        .foldZIO(
          err1 =>
            err1.validationErrorType match {
              case ValidationErrorType.MissingValue =>
                right.validate(args, conf).map { case (list, b) =>
                  (list, Right(b))
                }
              case _ =>
                ZIO.fail(ValidationError(ValidationErrorType.InvalidValue, err1.error))
            },
          r => ZIO.succeed((r._1, Left(r._2)))
        )

    override lazy val helpDoc: HelpDoc = left.helpDoc + right.helpDoc

    override lazy val uid: Option[String] = left.uid.toList ++ right.uid.toList match {
      case Nil  => None
      case list => Some(list.mkString(", "))
    }

    override val alternatives = List(left, right)
  }

  final case class Both[A, B](left: Options[A], right: Options[B]) extends Options[(A, B)] with Pipeline {
    override def modifySingle(f: SingleModifier): Options[(A, B)] =
      Both(left.modifySingle(f), right.modifySingle(f))

    lazy val synopsis: UsageSynopsis = left.synopsis + right.synopsis

    override def validate(args: List[String], conf: CliConfig): IO[ValidationError, (List[String], (A, B))] =
      for {
        tuple <- left
                   .validate(args, conf)
                   .catchAll(err1 =>
                     right
                       .validate(args, conf)
                       .foldZIO(
                         err2 => ZIO.fail(ValidationError(ValidationErrorType.MissingValue, err1.error + err2.error)),
                         _ => ZIO.fail(err1)
                       )
                   )
        (args, a) = tuple
        tuple    <- right.validate(args, conf)
        (args, b) = tuple
      } yield args -> (a -> b)

    override lazy val helpDoc: HelpDoc = left.helpDoc + right.helpDoc

    override lazy val uid: Option[String] = left.uid.toList ++ right.uid.toList match {
      case Nil  => None
      case list => Some(list.mkString(", "))
    }

    override def pipeline = ("", List(left, right))

  }

  final case class Map[A, B](value: Options[A], f: A => Either[ValidationError, B])
      extends Options[B]
      with Pipeline
      with Wrap {

    override lazy val shortDesc = value.shortDesc

    override def modifySingle(f0: SingleModifier): Options[B] = Map(value.modifySingle(f0), f)

    lazy val synopsis: UsageSynopsis = value.synopsis

    def validate(args: List[String], conf: CliConfig): IO[ValidationError, (List[String], B)] =
      value.validate(args, conf).flatMap(r => f(r._2).fold(e => ZIO.fail(e), s => ZIO.succeed(r._1 -> s)))

    override lazy val uid: Option[String] = value.uid

    override lazy val helpDoc: HelpDoc = value.helpDoc

    override def pipeline = ("", List(value))

    override val wrapped = value
  }

  final case class KeyValueMap(argumentOption: Options.Single[String])
      extends Options[Predef.Map[String, String]]
      with Input {

    override lazy val shortDesc: String = argumentOption.shortDesc

    override def helpDoc: HelpDoc = argumentOption.helpDoc

    override def synopsis: UsageSynopsis = argumentOption.synopsis

    override def uid: Option[String] = argumentOption.uid

    override def validate(
      args: List[String],
      conf: CliConfig
    ): IO[ValidationError, (List[String], Predef.Map[String, String])] = {

      def extractArgOptKeyValuePairs(
        input: List[String],
        conf: CliConfig
      ): (List[String], List[(String, String)]) = {

        val caseSensitive = conf.caseSensitive

        def withHyphen(argOpt: String) =
          (if (argOpt.length == 1) "-" else "--") +
            (if (caseSensitive) argOpt else argOpt.toLowerCase)

        val argOptSwitchNameAndAliases =
          (argumentOption.name +: argumentOption.aliases)
            .map(withHyphen)
            .toSet

        def isSwitch(s: String) = {
          val switch =
            if (caseSensitive) s
            else s.toLowerCase

          argOptSwitchNameAndAliases.contains(switch)
        }

        @tailrec
        def loop(
          acc: (List[String], List[(String, String)])
        ): (List[String], List[(String, String)]) = {
          val (input, pairs) = acc
          input match {
            case Nil => acc
            // `input` can be in the form of "-d key1=value1 -d key2=value2"
            case switch :: keyValueString :: tail if isSwitch(switch.trim) =>
              keyValueString.trim.split("=") match {
                case Array(key, value) =>
                  loop(tail -> ((key, value) :: pairs))
                case _ =>
                  acc
              }
            // Or, it can be in the form of "-d key1=value1 key2=value2"
            case keyValueString :: tail =>
              keyValueString.trim.split("=") match {
                case Array(key, value) =>
                  loop(tail -> ((key, value) :: pairs))
                case _ =>
                  acc
              }
            // Otherwise we give up and keep what remains as the leftover.
            case _ => acc
          }
        }

        loop(input -> Nil)
      }

      def processArguments(
        input: List[String],
        first: String,
        conf: CliConfig
      ): IO[ValidationError, (List[String], Predef.Map[String, String])] = (
        first.trim.split("=") match {
          case Array(key, value) =>
            ZIO.succeed(key -> value)
          case _ =>
            ZIO.fail(
              ValidationError(
                ValidationErrorType.InvalidArgument,
                p(error(s"Expected a key/value pair but got '$first'."))
              )
            )
        }
      ).map { first =>
        val (remains, pairs) = extractArgOptKeyValuePairs(input, conf)

        (remains, (first :: pairs).toMap)
      }

      argumentOption
        .validate(args, conf)
        .flatMap { case (input, first) =>
          processArguments(input, first, conf)
        }
    }

    override private[cli] def modifySingle(f: SingleModifier) =
      Options.keyValueMap(f(argumentOption))

    override def isValid(input: String, conf: CliConfig): IO[ValidationError, List[String]] =
      for {
        _ <- validate(uid.getOrElse("") :: input.split(" ").toList, conf)
      } yield uid.getOrElse("") :: input.split(" ").toList
  }

  final case class OAuth2Options(
    provider: OAuth2Provider,
    scope: List[String],
    auxiliaryOptions: Options[OAuth2AuxiliaryOptions]
  ) extends Options[OAuth2Token]
      with Wrap {
    override lazy val shortDesc: String = auxiliaryOptions.shortDesc

    override val wrapped = auxiliaryOptions

    override def helpDoc: HelpDoc = auxiliaryOptions.helpDoc

    override def synopsis: UsageSynopsis = auxiliaryOptions.synopsis

    override def uid: Option[String] = auxiliaryOptions.uid

    override def validate(args: List[String], conf: CliConfig): IO[ValidationError, (List[String], OAuth2Token)] =
      OAuth2PlatformSpecific.validate(provider, scope, auxiliaryOptions, args, conf)

    override private[cli] def modifySingle(f: SingleModifier): Options[OAuth2Token] =
      OAuth2Options(provider, scope, auxiliaryOptions.modifySingle(f))
  }

  /**
   * Creates a boolean flag with the specified name, which, if present, will produce the specified constant boolean
   * value.
   */
  def boolean(name: String, ifPresent: Boolean = true): Options[Boolean] = makeBoolean(name, ifPresent, Nil)

  /**
   * Creates a boolean flag with the specified name, which, if present, will produce the specified constant boolean
   * value. Negation names may be specified to explicitly invert the boolean value of this option.
   */
  def boolean(name: String, ifPresent: Boolean, negationName: String, negationNames: String*): Options[Boolean] =
    makeBoolean(name, ifPresent, negationName :: negationNames.toList)

  private def makeBoolean(name: String, ifPresent: Boolean, negationNames: List[String]): Options[Boolean] = {

    val option = Single(name, Vector.empty, PrimType.Bool(Some(ifPresent)))

    negationNames match {
      case Nil =>
        option.withDefault(!ifPresent)
      case head :: tail =>
        val negationOption = Single(head, tail.toVector, PrimType.Bool(Some(!ifPresent)))
        (option | negationOption).withDefault(!ifPresent)
    }
  }

  /**
   * Creates a parameter accepting one valye from set of allowed elements.
   */
  def enumeration[A](name: String)(cases: (String, A)*): Options[A] =
    Single(name, Vector.empty, PrimType.Enumeration(cases: _*))

  /**
   * Creates a parameter expecting path to the file.
   */
  def file(name: String, exists: Exists = Exists.Either): Options[JPath] =
    Single(name, Vector.empty, PrimType.Path(PathType.File, exists))

  /**
   * Creates a parameter expecting path to the directory.
   */
  def directory(name: String, exists: Exists = Exists.Either): Options[JPath] =
    Single(name, Vector.empty, PrimType.Path(PathType.Directory, exists))

  /**
   * Creates a parameter expecting an arbitrary text.
   */
  def text(name: String): Options[String] =
    Single(name, Vector.empty, PrimType.Text)

  /**
   * Creates a parameter expecting a decimal number.
   */
  def decimal(name: String): Options[BigDecimal] =
    Single(name, Vector.empty, PrimType.Decimal)

  /**
   * Creates a parameter expecting an integer.
   */
  def integer(name: String): Options[BigInt] =
    Single(name, Vector.empty, PrimType.Integer)

  /**
   * Creates a parameter expecting a parameter for instant in time in UTC format, such as 2007-12-03T10:15:30.00Z.
   */
  def instant(name: String): Options[JInstant] =
    Single(name, Vector.empty, PrimType.Instant)

  /**
   * Creates a parameter excepting parameter for a date in ISO_LOCAL_DATE format, such as 2007-12-03.
   */
  def localDate(name: String): Options[JLocalDate] =
    Single(name, Vector.empty, PrimType.LocalDate)

  /**
   * Creates a parameter excepting a date-time without a time-zone in the ISO-8601 format, such as 2007-12-03T10:15:30.
   */
  def localDateTime(name: String): Options[JLocalDateTime] =
    Single(name, Vector.empty, PrimType.LocalDateTime)

  /**
   * Creates a parameter excepting a time without a time-zone in the ISO-8601 format, such as 10:15:30.
   */
  def localTime(name: String): Options[JLocalTime] =
    Single(name, Vector.empty, PrimType.LocalTime)

  /**
   * Creates a parameter excepting a month-day in the ISO-8601 format such as 12-03.
   */
  def monthDay(name: String): Options[JMonthDay] =
    Single(name, Vector.empty, PrimType.MonthDay)

  val none: Options[Unit] = Empty

  /**
   * Creates a parameter excepting a date-time with an offset from UTC/Greenwich in the ISO-8601 format, such as
   * 2007-12-03T10:15:30+01:00.
   */
  def offsetDateTime(name: String): Options[JOffsetDateTime] =
    Single(name, Vector.empty, PrimType.OffsetDateTime)

  /**
   * Creates a parameter excepting a time with an offset from UTC/Greenwich in the ISO-8601 format, such as
   * 10:15:30+01:00.
   */
  def offsetTime(name: String): Options[JOffsetTime] =
    Single(name, Vector.empty, PrimType.OffsetTime)

  /**
   * Creates a parameter excepting a date-based amount of time in the ISO-8601 format, such as 'P1Y2M3D'.
   */
  def period(name: String): Options[JPeriod] =
    Single(name, Vector.empty, PrimType.Period)

  /**
   * Creates a parameter expecting a year in the ISO-8601 format, such as 2007.
   */
  def year(name: String): Options[JYear] =
    Single(name, Vector.empty, PrimType.Year)

  /**
   * Creates a parameter expecting a year-month in the ISO-8601 format, such as 2007-12..
   */
  def yearMonth(name: String): Options[JYearMonth] =
    Single(name, Vector.empty, PrimType.YearMonth)

  /**
   * Creates a date-time with a time-zone in the ISO-8601 format, such as 2007-12-03T10:15:30+01:00 Europe/Paris.
   */
  def zonedDateTime(name: String): Options[JZonedDateTime] =
    Single(name, Vector.empty, PrimType.ZonedDateTime)

  /**
   * Creates a parameter expecting a time-zone ID, such as Europe/Paris.
   */
  def zoneId(name: String): Options[JZoneId] =
    Single(name, Vector.empty, PrimType.ZoneId)

  /**
   * Creates a parameter expecting a time-zone offset from Greenwich/UTC, such as +02:00.
   */
  def zoneOffset(name: String): Options[JZoneOffset] =
    Single(name, Vector.empty, PrimType.ZoneOffset)

  /**
   * Creates a property flag with the specified name. Property arguments may be repeated several times (-D key1=value -D
   * key2=value) or specifying all key/values in one argument (-D key1=value key2=value).
   */
  def keyValueMap(name: String): Options[Predef.Map[String, String]] =
    keyValueMap(Options.Single(name, Vector.empty, PrimType.Text))

  /**
   * Creates a property flag with from an argument option as Options.single.
   */
  def keyValueMap(argumentOption: Options.Single[String]): Options[Predef.Map[String, String]] =
    Options.KeyValueMap(argumentOption)
}
