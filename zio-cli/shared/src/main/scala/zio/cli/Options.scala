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
sealed trait Options[+A] extends Parameter { self =>

  import Options.Single

  final def ++[A1 >: A, B](that: Options[B])(implicit zippable: Zippable[A, B]): Options[zippable.Out] =
    Options.Both(self, that).map { case (a, b) => zippable.zip(a, b) }

  final def |[A1 >: A](that: Options[A1]): Options[A1] = self.orElse(that)

  final def orElse[A1 >: A](that: Options[A1]): Options[A1] =
    self.orElseEither(that).map(_.merge)

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

  // TODO : spend time to understand usage of implicit here

  final def as[B, C, Z](f: (B, C) => Z)(implicit ev: A <:< (B, C)): Options[Z] =
    self.map(ev).map { case (b, c) => f(b, c) }

  final def as[B, C, D, Z](f: (B, C, D) => Z)(implicit ev: A <:< (B, C, D)): Options[Z] =
    self.map(ev).map { case (b, c, d) => f(b, c, d) }

  final def as[B, C, D, E, Z](f: (B, C, D, E) => Z)(implicit ev: A <:< (B, C, D, E)): Options[Z] =
    self.map(ev).map { case (b, c, d, e) => f(b, c, d, e) }

  final def as[B, C, D, E, F, Z](f0: (B, C, D, E, F) => Z)(implicit ev: A <:< (B, C, D, E, F)): Options[Z] =
    self.map(ev).map { case (b, c, d, e, f) => f0(b, c, d, e, f) }

  final def as[B, C, D, E, F, G, Z](
    f0: (B, C, D, E, F, G) => Z
  )(implicit ev: A <:< (B, C, D, E, F, G)): Options[Z] =
    self.map(ev).map { case (b, c, d, e, f, g) => f0(b, c, d, e, f, g) }

  final def fold[B, C, Z](
    f1: B => Z,
    f2: C => Z
  )(implicit ev: A <:< Either[B, C]): Options[Z] =
    self.map(ev).map {
      case Left(b)  => f1(b)
      case Right(c) => f2(c)
    }

  final def fold[B, C, D, Z](
    f1: B => Z,
    f2: C => Z,
    f3: D => Z
  )(implicit ev: A <:< Either[Either[B, C], D]): Options[Z] =
    self.map(ev).map {
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
    self.map(ev).map {
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
    self.map(ev).map {
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
    self.map(ev).map {
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
    self.asInstanceOf[Options[_]] match {
      case Options.Empty                   => false
      case Options.WithDefault(options, _) => options.isBool
      case Single(_, _, primType, _)       => primType.isBool
      case Options.Map(value, _)           => value.isBool
      case _                               => false
    }

  final def map[B](f: A => B): Options[B] = Options.Map(self, (a: A) => Right(f(a)))

  final def mapOrFail[B](f: A => Either[ValidationError, B]): Options[B] =
    Options.Map(self, (a: A) => f(a))

  final def mapTry[B](f: A => B): Options[B] =
    self.mapOrFail((a: A) =>
      scala.util.Try(f(a)).toEither.left.map(e => ValidationError(ValidationErrorType.InvalidValue, p(e.getMessage)))
    )
  final def optional: Options[Option[A]] = self.map(Some(_)).withDefault(None)

  final def primitiveType: Option[PrimType[A]] =
    self match {
      case Single(_, _, primType, _) => Some(primType)
      case _                         => None
    }

  def synopsis: UsageSynopsis

  def uid: Option[String]

  def validate(args: List[String], conf: CliConfig): IO[ValidationError, (List[String], A)]

  def withDefault[A1 >: A](value: A1): Options[A1] =
    Options.WithDefault(self, value)

  private[cli] def modifySingle(f: SingleModifier): Options[A]

  lazy val tag = "option"
}

trait SingleModifier {
  def apply[A](single: Options.Single[A]): Options.Single[A]
}

object Options extends OptionsPlatformSpecific {
  case object Empty extends Options[Unit] with Pipeline { self =>
    lazy val synopsis: UsageSynopsis = UsageSynopsis.None

    def validate(args: List[String], conf: CliConfig): IO[ValidationError, (List[String], Unit)] =
      ZIO.succeed((args, ()))

    override def modifySingle(f: SingleModifier): Options[Unit] = Empty

    override lazy val helpDoc: HelpDoc = HelpDoc.Empty

    override lazy val uid: Option[String] = None

    override def pipeline = ("", List())
  }

  final case class WithDefault[A](options: Options[A], default: A) extends Options[A] with Input { self =>

    override lazy val shortDesc: String = options.shortDesc

    lazy val synopsis: UsageSynopsis = self.options.synopsis.optional

    def validate(args: List[String], conf: CliConfig): IO[ValidationError, (List[String], A)] =
      self.options
        .validate(args, conf)
        .catchSome {
          case error if error.isOptionMissing => ZIO.succeed(args -> self.default)
        }

    override def modifySingle(f: SingleModifier): Options[A] =
      WithDefault(self.options.modifySingle(f), self.default)

    override lazy val helpDoc: HelpDoc =
      options.helpDoc.mapDescriptionList { case (span, block) =>
        span -> (block + HelpDoc.p(s"This setting is optional. Default: '${self.default}'."))
      }

    override lazy val uid: Option[String] = self.options.uid

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
    description: HelpDoc = HelpDoc.Empty
  ) extends Options[A]
      with Input { self =>

    override lazy val shortDesc: String = s"""Option "$name". ${description.getSpan.text}"""

    override def modifySingle(f: SingleModifier): Options[A] = f(self)

    lazy val synopsis: UsageSynopsis =
      UsageSynopsis.Named(
        self.names,
        if (!self.primType.isBool) self.primType.choices.orElse(Some(self.primType.typeName)) else None
      )

    def unCluster(args: List[String]): List[String] = {
      def isClusteredOption(value: String): Boolean = value.trim.matches("^-{1}([^-]{2,}$)")

      args.flatMap { arg =>
        if (isClusteredOption(arg))
          arg.substring(1).map(c => s"-$c")
        else arg :: Nil
      }
    }

    def validate(args: List[String], conf: CliConfig): IO[ValidationError, (List[String], A)] =
      args match {
        case head :: tail =>
          if (head.trim.matches("^-{1}([^-]{2,}$)")) 
            validate(head.drop(1).map(c => s"-$c") :: tail, conf).catchSome {
              case _: ValidationError => validate(tail, conf).map {
                case (rest, a) => (head :: rest, a)
              }
            }
          else {
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
          } else {
            validate(rest, conf).map { case (args, a) =>
              (head :: args, a)
            }.catchSome {
              case e: ValidationError => if (
                  name.length > conf.autoCorrectLimit + 1 && 
                  AutoCorrect.levensteinDistance(head, fullName, conf) <= conf.autoCorrectLimit
                ) ZIO.fail(
                ValidationError(
                  ValidationErrorType.InvalidValue,
                  p(error(s"""The flag "$head" is not recognized. Did you mean ${fullName}?"""))
                )
              ) else ZIO.fail(e)
            }
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

      if (conf.caseSensitive) process(self.names.contains)
      else process(s => self.names.exists(_.equalsIgnoreCase(s)))
    }

    lazy val uid: Option[String] = Some(self.fullName)

    private def makeFullName(s: String): (Boolean, String) = if (s.length == 1) (true, "-" + s) else (false, "--" + s)

    lazy val names: List[String] = (self.name :: self.aliases.toList).map(self.makeFullName).sortBy(!_._1).map(_._2)

    private lazy val fullName: String = self.makeFullName(self.name)._2

    override lazy val helpDoc: HelpDoc =
      HelpDoc.DescriptionList(List(self.synopsis.helpDoc.getSpan -> (p(self.primType.helpDoc) + self.description)))

    override def isValid(input: String, conf: CliConfig): IO[ValidationError, List[String]] = for {
      _ <- validate(List(self.names.head, input), conf)
    } yield List(self.names.head, input)
  }

  final case class OrElse[A, B](left: Options[A], right: Options[B]) extends Options[Either[A, B]] with Alternatives {
    self =>
    override def modifySingle(f: SingleModifier): Options[Either[A, B]] =
      OrElse(left.modifySingle(f), self.right.modifySingle(f))

    lazy val synopsis: UsageSynopsis = UsageSynopsis.Alternation(self.left.synopsis, self.right.synopsis)

    override def validate(args: List[String], conf: CliConfig): IO[ValidationError, (List[String], Either[A, B])] =
      self.left
        .validate(args, conf)
        .foldZIO(
          err1 =>
            self.right
              .validate(args, conf)
              .foldZIO[Any, ValidationError, (List[String], Either[A, B])](
                err2 =>
                  ZIO.fail(
                    // orElse option is only missing in case neither option was given
                    (err1.validationErrorType, err2.validationErrorType) match {
                      case (ValidationErrorType.MissingValue, ValidationErrorType.MissingValue) =>
                        ValidationError(ValidationErrorType.MissingValue, err1.error + err2.error)
                      case _ =>
                        ValidationError(ValidationErrorType.InvalidValue, err1.error + err2.error)
                    }
                  ),
                success => ZIO.succeed((success._1, Right(success._2)))
              ),
          r =>
            self.right
              .validate(r._1, conf)
              .foldZIO(
                _ => ZIO.succeed((r._1, Left(r._2))),
                _ =>
                  ZIO.fail(
                    ValidationError(
                      ValidationErrorType.InvalidValue,
                      p(
                        error(s"Options collision detected. You can only specify either ${self.left} or ${self.right}.")
                      )
                    )
                  )
              )
        )

    override lazy val helpDoc: HelpDoc = self.left.helpDoc + self.right.helpDoc

    override lazy val uid: Option[String] = self.left.uid.toList ++ self.right.uid.toList match {
      case Nil  => None
      case list => Some(list.mkString(", "))
    }

    override val alternatives = List(left, right)
  }

  final case class Both[A, B](left: Options[A], right: Options[B]) extends Options[(A, B)] with Pipeline { self =>
    override def modifySingle(f: SingleModifier): Options[(A, B)] =
      Both(self.left.modifySingle(f), self.right.modifySingle(f))

    lazy val synopsis: UsageSynopsis = self.left.synopsis + self.right.synopsis

    override def validate(args: List[String], conf: CliConfig): IO[ValidationError, (List[String], (A, B))] =
      for {
        tuple <- self.left
                   .validate(args, conf)
                   .catchAll(err1 =>
                     self.right
                       .validate(args, conf)
                       .foldZIO(
                         err2 => ZIO.fail(ValidationError(ValidationErrorType.MissingValue, err1.error + err2.error)),
                         _ => ZIO.fail(err1)
                       )
                   )
        (args, a) = tuple
        tuple    <- self.right.validate(args, conf)
        (args, b) = tuple
      } yield args -> (a -> b)

    override lazy val helpDoc: HelpDoc = self.left.helpDoc + self.right.helpDoc

    override lazy val uid: Option[String] = self.left.uid.toList ++ self.right.uid.toList match {
      case Nil  => None
      case list => Some(list.mkString(", "))
    }

    override def pipeline = ("", List(self.left, self.right))

  }

  final case class Map[A, B](value: Options[A], f: A => Either[ValidationError, B])
      extends Options[B]
      with Pipeline
      with Wrap { self =>

    override lazy val shortDesc = value.shortDesc

    override def modifySingle(f0: SingleModifier): Options[B] = Map(self.value.modifySingle(f0), self.f)

    lazy val synopsis: UsageSynopsis = self.value.synopsis

    def validate(args: List[String], conf: CliConfig): IO[ValidationError, (List[String], B)] =
      self.value.validate(args, conf).flatMap(r => self.f(r._2).fold(e => ZIO.fail(e), s => ZIO.succeed(r._1 -> s)))

    override lazy val uid: Option[String] = self.value.uid

    override lazy val helpDoc: HelpDoc = self.value.helpDoc

    override def pipeline = ("", List(value))

    override val wrapped = value
  }

  final case class KeyValueMap(argumentOption: Options.Single[String])
      extends Options[Predef.Map[String, String]]
      with Input {
    self =>

    override lazy val shortDesc: String = argumentOption.shortDesc

    override def helpDoc: HelpDoc = self.argumentOption.helpDoc

    override def synopsis: UsageSynopsis = self.argumentOption.synopsis

    override def uid: Option[String] = self.argumentOption.uid

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
          (self.argumentOption.name +: self.argumentOption.aliases)
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

      self.argumentOption
        .validate(args, conf)
        .flatMap { case (input, first) =>
          processArguments(input, first, conf)
        }
    }

    override private[cli] def modifySingle(f: SingleModifier) =
      Options.keyValueMap(f(self.argumentOption))

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

    override val wrapped                 = auxiliaryOptions
    override def helpDoc: HelpDoc        = auxiliaryOptions.helpDoc
    override def synopsis: UsageSynopsis = auxiliaryOptions.synopsis
    override def uid: Option[String]     = auxiliaryOptions.uid
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
