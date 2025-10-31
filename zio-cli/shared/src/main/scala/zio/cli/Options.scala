package zio.cli

import zio.cli.HelpDoc.Span._
import zio.cli.HelpDoc.p
import zio.{IO, UIO, ZIO, Zippable}
import zio.cli.oauth2._

import java.time.{
  Duration => JDuration,
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
import zio.cli.PrimType.Bool

/**
 * A `Flag[A]` models a command-line flag that produces a value of type `A`.
 */
sealed trait Options[+A] extends Parameter { self =>

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
      override def apply[A1](single: Single[A1]): Single[A1] = single.copy(description = single.description + p(that))
    })

  def alias(name: String, names: String*): Options[A] =
    modifySingle(new SingleModifier {
      override def apply[A1](single: Single[A1]): Single[A1] = single.copy(aliases = single.aliases ++ (name +: names))
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

  def flatten: List[Options[_] with Input]

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

  // Value `unclustered` is true if the first `String` represent an option that has been unclustered.
  def validate(args: Predef.Map[String, List[String]], conf: CliConfig): IO[ValidationError, A]

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

  /**
   * Returns the leftover arguments, leftover options and a `Map`linking the first argument with its values if it
   * corresponds to an option flag.
   */
  private def findOptions(
    input: List[String],
    options: List[Options[_] with Input],
    conf: CliConfig
  ): IO[ValidationError, (List[String], List[Options[_] with Input], Predef.Map[String, List[String]])] =
    options match {
      case Nil          => ZIO.succeed((input, Nil, Predef.Map.empty))
      case head :: tail =>
        head
          .parse(input, conf)
          .flatMap {
            case (Nil, input) =>
              findOptions(input, tail, conf).map { case (otherArgs, otherOptions, map) =>
                (otherArgs, head :: otherOptions, map)
              }
            case (name :: values, leftover) =>
              ZIO.succeed((leftover, tail, Predef.Map(name -> values)))
          }
          .catchSome { case e @ ValidationError(validationErrorType, _) =>
            validationErrorType match {
              case ValidationErrorType.UnclusteredFlag(list, tail) =>
                matchUnclustered(list, tail, options, conf).orElseFail(e)
              case ValidationErrorType.MissingFlag =>
                findOptions(input, tail, conf).map { case (otherArgs, otherOptions, map) =>
                  (otherArgs, head :: otherOptions, map)
                }
              case ValidationErrorType.CorrectedFlag =>
                for {
                  tuple                         <- findOptions(input, tail, conf).orElseFail(e)
                  (otherArgs, otherOptions, map) = tuple
                  _                             <- ZIO.when(map.isEmpty)(ZIO.fail(e))
                } yield (otherArgs, head :: otherOptions, map)
              case _ => ZIO.fail(e)
            }

          }
    }

  private def matchUnclustered(
    input: List[String],
    tail: List[String],
    options: List[Options[_] with Input],
    conf: CliConfig
  ): IO[ValidationError, (List[String], List[Options[_] with Input], Predef.Map[String, List[String]])] =
    input match {
      case Nil                => ZIO.succeed((tail, options, Predef.Map.empty))
      case flag :: otherFlags =>
        for {
          tuple1          <- findOptions(flag :: Nil, options, conf)
          (_, opts1, map1) = tuple1
          tuple2          <-
            if (map1.isEmpty) ZIO.fail(ValidationError(ValidationErrorType.UnclusteredFlag(Nil, tail), HelpDoc.empty))
            else matchUnclustered(otherFlags, tail, opts1, conf)
          (_, opts2, map2) = tuple2
        } yield (tail, opts2, merge(map1, map2.toList))
    }

  // Sums the list associated with the same key.
  @tailrec
  private def merge(
    map1: Predef.Map[String, List[String]],
    map2: List[(String, List[String])]
  ): Predef.Map[String, List[String]] =
    map2 match {
      case Nil          => map1
      case head :: tail =>
        // replace with updatedWith for Scala 2.13
        val newMap = map1.get(head._1) match {
          case None       => map1 + head
          case Some(elem) => map1.updated(head._1, elem ++ head._2)
        }
        merge(newMap, tail)
    }

  /**
   * Returns a possible `ValidationError` when parsing the commands, leftover arguments from `input` and a `Map` linking
   * each flag with its values.
   */
  private def matchOptions(
    input: List[String],
    options: List[Options[_] with Input],
    conf: CliConfig
  ): UIO[(Option[ValidationError], List[String], Predef.Map[String, List[String]])] =
    (input, options) match {
      case (Nil, _)         => ZIO.succeed((None, Nil, Predef.Map.empty))
      case (_, Nil)         => ZIO.succeed((None, input, Predef.Map.empty))
      case (input, options) =>
        (for {
          tuple1                         <- findOptions(input, options, conf)
          (otherArgs, otherOptions, map1) = tuple1
          tuple2                         <-
            if (map1.isEmpty) ZIO.succeed((None, input, map1))
            else matchOptions(otherArgs, otherOptions, conf)
          (error, otherArgs, map2) = tuple2
        } yield (error, otherArgs, merge(map1, map2.toList)))
          .catchAll(e => ZIO.succeed((Some(e), input, Predef.Map.empty)))
    }

  /**
   * `Options.validate` parses `args` for options and returns an `Option[ValidationError]`, the leftover arguments and
   * the constructed value of type `A`. The possible error inside `Option[ValidationError]` would only be triggered if
   * there is an error when parsing the `Args` of a `Command`. This is because `ValidationErrors` are used to control
   * the end of the args corresponding to options.
   */
  def validate[A](
    options: Options[A],
    args: List[String],
    conf: CliConfig
  ): IO[ValidationError, (Option[ValidationError], List[String], A)] =
    for {
      matched                             <- matchOptions(args, options.flatten, conf)
      (error, commandArgs, matchedOptions) = matched
      a                                   <- options.validate(matchedOptions, conf).mapError(error.getOrElse(_))
    } yield (error, commandArgs, a)

  case object Empty extends Options[Unit] with Pipeline {
    lazy val synopsis: UsageSynopsis = UsageSynopsis.None

    override def flatten: List[Options[_] with Input] = Nil

    override def validate(args: Predef.Map[String, List[String]], conf: CliConfig): IO[ValidationError, Unit] = ZIO.unit

    override def modifySingle(f: SingleModifier): Options[Unit] = Empty

    override lazy val helpDoc: HelpDoc = HelpDoc.Empty

    override lazy val uid: Option[String] = None

    override def pipeline = ("", List())
  }

  final case class WithDefault[A](options: Options[A], default: A) extends Options[A] with Input {

    override lazy val shortDesc: String = options.shortDesc

    lazy val synopsis: UsageSynopsis = options.synopsis.optional

    override def flatten: List[Options[_] with Input] = options.flatten

    override def parse(args: List[String], conf: CliConfig): IO[ValidationError, (List[String], List[String])] =
      ZIO.fail(
        ValidationError(ValidationErrorType.CommandMismatch, p(error("Error in command design")))
      )

    override def validate(args: Predef.Map[String, List[String]], conf: CliConfig): IO[ValidationError, A] =
      options
        .validate(args, conf)
        .catchSome {
          case error if error.isOptionMissing => ZIO.succeed(default)
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
      with Input { self =>

    override lazy val shortDesc: String = s"""Option "$name". ${description.getSpan.text}"""

    override def modifySingle(f: SingleModifier): Options[A] = f(self)

    lazy val synopsis: UsageSynopsis =
      UsageSynopsis.Named(
        names,
        if (!primType.isBool) primType.choices.orElse(Some(placeholder)) else None
      )

    override def flatten: List[Options[_] with Input] = List(self)

    override def validate(args: Predef.Map[String, List[String]], conf: CliConfig): IO[ValidationError, A] =
      names.map(args.get).flatMap {
        case None    => Nil
        case Some(x) => List(x)
      } match {
        case Nil =>
          ZIO.fail(
            ValidationError(ValidationErrorType.MissingValue, p(error(s"Expected to find $fullName option.")))
          )
        case head :: Nil =>
          head match {
            case Nil =>
              primType
                .validate(None, conf)
                .mapError(e => ValidationError(ValidationErrorType.InvalidValue, p(e)))
            case head :: Nil =>
              primType
                .validate(head, conf)
                .mapError(e => ValidationError(ValidationErrorType.InvalidValue, p(e)))
            case list => ZIO.fail(ValidationError(ValidationErrorType.KeyValuesDetected(list), HelpDoc.empty))
          }
        case _ =>
          ZIO.fail(
            ValidationError(
              ValidationErrorType.InvalidValue,
              p(error(s"""More than one reference to option $fullName."""))
            )
          )
      }

    override def parse(args: List[String], conf: CliConfig): IO[ValidationError, (List[String], List[String])] =
      processArgs(args).flatMap {
        case head :: tail =>
          if (names.map(conf.normalizeCase).contains(conf.normalizeCase(head))) {
            primType match {
              case _: Bool =>
                tail match {
                  case "true" :: tail  => ZIO.succeed((head :: "true" :: Nil, tail))
                  case "false" :: tail => ZIO.succeed((head :: "false" :: Nil, tail))
                  case _               => ZIO.succeed((head :: Nil, tail))
                }
              case _ =>
                tail match {
                  case value :: tail => ZIO.succeed((List(head, value), tail))
                  case Nil           =>
                    ZIO.fail(
                      ValidationError(ValidationErrorType.MissingValue, p(error(s"Expected some value.")))
                    )
                }
            }
          } else if (
            name.length > conf.autoCorrectLimit + 1 &&
            AutoCorrect.levensteinDistance(head, fullName, conf) <= conf.autoCorrectLimit
          )
            ZIO.fail(
              ValidationError(
                ValidationErrorType.CorrectedFlag,
                p(error(s"""The flag "$head" is not recognized. Did you mean $fullName?"""))
              )
            )
          else
            ZIO.fail(
              ValidationError(
                ValidationErrorType.MissingFlag,
                p(error(s"Expected to find $fullName option."))
              )
            )
        case Nil =>
          ZIO.fail(
            ValidationError(ValidationErrorType.MissingFlag, p(error(s"Expected to find $fullName option.")))
          )
      }

    private def processArgs(args: List[String]): IO[ValidationError, List[String]] =
      args match {
        case Nil          => ZIO.succeed(Nil)
        case head :: tail =>
          if (head.trim.matches("^-{1}([^-]{2,}$)"))
            ZIO.fail(
              ValidationError(
                ValidationErrorType.UnclusteredFlag(
                  head.substring(1).map(c => s"-$c").toList,
                  tail
                ),
                HelpDoc.empty
              )
            )
          else if (head.startsWith("--")) {
            val splitArg = head.span(_ != '=')
            if (splitArg._2 != "") ZIO.succeed(splitArg._1 :: splitArg._2.tail :: tail)
            else ZIO.succeed(args)
          } else ZIO.succeed(args)
      }

    lazy val uid: Option[String] = Some(fullName)

    private def makeFullName(s: String): (Boolean, String) = if (s.length == 1) (true, "-" + s) else (false, "--" + s)

    lazy val names: List[String] = (name :: aliases.toList).map(makeFullName).sortBy(!_._1).map(_._2)

    private lazy val fullName: String = makeFullName(name)._2

    override lazy val helpDoc: HelpDoc =
      HelpDoc.DescriptionList(List(synopsis.helpDoc.getSpan -> (p(primType.helpDoc) + description)))

    override def isValid(input: String, conf: CliConfig): IO[ValidationError, List[String]] = for {
      _ <- parse(List(names.head, input), conf)
    } yield List(names.head, input)

    private def placeholder: String = "<" + pseudoName.getOrElse(primType.typeName) + ">"
  }

  final case class OrElse[A, B](left: Options[A], right: Options[B]) extends Options[Either[A, B]] with Alternatives {

    override def modifySingle(f: SingleModifier): Options[Either[A, B]] =
      OrElse(left.modifySingle(f), right.modifySingle(f))

    lazy val synopsis: UsageSynopsis = UsageSynopsis.Alternation(left.synopsis, right.synopsis)

    override def flatten: List[Options[_] with Input] = left.flatten ++ right.flatten

    override def validate(args: Predef.Map[String, List[String]], conf: CliConfig): IO[ValidationError, Either[A, B]] =
      left
        .validate(args, conf)
        .foldZIO(
          err1 =>
            right
              .validate(args, conf)
              .mapBoth(
                err2 =>
                  // orElse option is only missing in case neither option was given
                  (err1.validationErrorType, err2.validationErrorType) match {
                    case (ValidationErrorType.MissingValue, ValidationErrorType.MissingValue) =>
                      ValidationError(ValidationErrorType.MissingValue, err1.error + err2.error)
                    case _ =>
                      ValidationError(ValidationErrorType.InvalidValue, err1.error + err2.error)
                  },
                b => Right(b)
              ),
          a =>
            right
              .validate(args, conf)
              .foldZIO(
                _ => ZIO.succeed(Left(a)),
                _ => {
                  // `uid` will only be None for Options.Empty. Which means the user would
                  // have had to purposefully compose Options.Empty | otherArgument.
                  val leftUid  = left.uid.getOrElse("???")
                  val rightUid = right.uid.getOrElse("???")
                  ZIO.fail(
                    ValidationError(
                      ValidationErrorType.InvalidValue,
                      p(error(s"Options collision detected. You can only specify either $leftUid or $rightUid."))
                    )
                  )
                }
              )
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

    override def flatten: List[Options[_] with Input] = left.flatten ++ right.flatten

    override def validate(args: Predef.Map[String, List[String]], conf: CliConfig): IO[ValidationError, (A, B)] =
      for {
        a <- left
               .validate(args, conf)
               .catchAll(err1 =>
                 right
                   .validate(args, conf)
                   .foldZIO(
                     err2 => ZIO.fail(ValidationError(ValidationErrorType.MissingValue, err1.error + err2.error)),
                     _ => ZIO.fail(err1)
                   )
               )
        b <- right.validate(args, conf)
      } yield (a -> b)

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

    override def flatten: List[Options[_] with Input] = value.flatten

    def validate(args: Predef.Map[String, List[String]], conf: CliConfig): IO[ValidationError, B] =
      value.validate(args, conf).flatMap(a => f(a).fold(e => ZIO.fail(e), s => ZIO.succeed(s)))

    override lazy val uid: Option[String] = value.uid

    override lazy val helpDoc: HelpDoc = value.helpDoc

    override def pipeline = ("", List(value))

    override val wrapped = value
  }

  final case class KeyValueMap(argumentOption: Options.Single[String])
      extends Options[Predef.Map[String, String]]
      with Input { self =>

    override lazy val shortDesc: String = argumentOption.shortDesc

    override def helpDoc: HelpDoc = argumentOption.helpDoc

    override def synopsis: UsageSynopsis = argumentOption.synopsis

    override def flatten: List[Options[_] with Input] = List(self)

    override def uid: Option[String] = argumentOption.uid

    override def validate(
      args: Predef.Map[String, List[String]],
      conf: CliConfig
    ): IO[ValidationError, Predef.Map[String, String]] = {

      def extractKeyValue(
        keyValue: String
      ): IO[ValidationError, (String, String)] =
        keyValue.trim.span(_ != '=') match {
          case (_, "") =>
            ZIO.fail(
              ValidationError(
                ValidationErrorType.InvalidArgument,
                p(error(s"Expected a key/value pair but got '$keyValue'."))
              )
            )
          case (key, value) =>
            ZIO.succeed((key -> value.tail))
        }

      argumentOption
        .validate(args, conf)
        .foldZIO(
          { case e @ ValidationError(errorType, _) =>
            errorType match {
              case ValidationErrorType.KeyValuesDetected(list) =>
                ZIO
                  .foreach(list)(extractKeyValue)
                  .map(_.toMap)
              case _ =>
                ZIO.fail(e)
            }
          },
          extractKeyValue(_).map(List(_).toMap)
        )
    }

    override def parse(args: List[String], conf: CliConfig): IO[ValidationError, (List[String], List[String])] = {

      lazy val names = argumentOption.names.map(conf.normalizeCase)

      @tailrec
      def loop(
        acc: (List[String], List[String])
      ): (List[String], List[String]) = {
        val (pairs, input) = acc
        input match {
          case Nil => acc
          // `input` can be in the form of "-d key1=value1 -d key2=value2"
          case switch :: keyValueString :: tail if names.contains(conf.normalizeCase(switch)) =>
            keyValueString.trim.span(_ != '=') match {
              case (_, "")  => acc
              case (_, "=") => acc
              case _        =>
                loop((keyValueString.trim :: pairs) -> tail)
            }
          // Or, it can be in the form of "-d key1=value1 key2=value2"
          case keyValueString :: tail =>
            keyValueString.trim.span(_ != '=') match {
              case (_, "")  => acc
              case (_, "=") => acc
              case _        =>
                loop((keyValueString.trim :: pairs) -> tail)
            }
          // Otherwise we give up and keep what remains as the leftover.
          case _ => acc
        }
      }

      args match {
        case switch :: tail if names.contains(switch) =>
          loop(Nil -> tail) match {
            case (Nil, _)              => ZIO.succeed(Nil -> args)
            case (keyValues, leftover) => ZIO.succeed((switch :: keyValues) -> leftover)
          }
        case _ => ZIO.succeed(Nil -> args)
      }

    }

    override private[cli] def modifySingle(f: SingleModifier) =
      Options.keyValueMap(f(argumentOption))

    override def isValid(input: String, conf: CliConfig): IO[ValidationError, List[String]] =
      for {
        _ <- validate(Predef.Map(uid.getOrElse("") -> input.split(" ").toList), conf)
      } yield uid.getOrElse("") :: input.split(" ").toList
  }

  final case class OAuth2Options(
    provider: OAuth2Provider,
    scope: List[String],
    auxiliaryOptions: Options[OAuth2AuxiliaryOptions]
  ) extends Options[OAuth2Token]
      with Wrap {
    override lazy val shortDesc: String = auxiliaryOptions.shortDesc

    override val wrapped                                                                                             = auxiliaryOptions
    override def helpDoc: HelpDoc                                                                                    = auxiliaryOptions.helpDoc
    override def synopsis: UsageSynopsis                                                                             = auxiliaryOptions.synopsis
    override def flatten: List[Options[_] with Input]                                                                = auxiliaryOptions.flatten
    override def uid: Option[String]                                                                                 = auxiliaryOptions.uid
    override def validate(args: Predef.Map[String, List[String]], conf: CliConfig): IO[ValidationError, OAuth2Token] =
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

  /**
   * Creates a boolean flag with the specified name, which, if present, will produce the specified constant boolean
   * value. Negation names may be specified to explicitly invert the boolean value of this option. An alias might be
   * specified to substitute the name.
   */
  def boolean(
    name: String,
    alias: String,
    ifPresent: Boolean,
    negationName: String,
    negationNames: String*
  ): Options[Boolean] =
    makeBoolean(name, ifPresent, negationName :: negationNames.toList, alias :: Nil)

  private def makeBoolean(
    name: String,
    ifPresent: Boolean,
    negationNames: List[String],
    aliases: List[String] = Nil
  ): Options[Boolean] = {

    val option = Single(name, aliases.toVector, PrimType.Bool(Some(ifPresent)))

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
   * Creates a parameter excepting a date-based amount of time in the ISO-8601 format, such as 'P1DT2H3M'.
   */
  def duration(name: String): Options[JDuration] =
    Single(name, Vector.empty, PrimType.Duration)

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
