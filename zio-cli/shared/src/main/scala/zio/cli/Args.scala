package zio.cli

import zio._
import zio.cli.HelpDoc.{Span, p}

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

/**
 * An `Args[A]` describes the positional arguments of a command: the words the user types after the command name and its
 * options. Parsing them produces a value of type `A`.
 *
 * Create one with a constructor such as `Args.text("file")` or `Args.integer`, combine several with `++`, repeat one
 * with `*`, `+`, `atLeast`, `atMost` or `between`, and transform its value with `map`. On the command line, a command's
 * arguments come after its options.
 *
 * {{{
 * val source: Args[String]                = Args.text("source") ?? "The file to copy."
 * val targets: Args[List[String]]         = Args.text("target").atLeast(1)
 * val both: Args[(String, List[String])]  = source ++ targets
 * }}}
 *
 * @see
 *   [[https://zio.dev/zio-cli/args/ Args]]
 */
sealed trait Args[+A] extends Parameter { self =>

  /**
   * Combines this argument with `that`, which is parsed next from the words this one leaves. The two values are zipped
   * into a flat tuple (`a ++ b ++ c` gives a triple), and a `Unit` value is dropped (`Args.none ++ Args.text` gives an
   * `Args[String]`).
   */
  final def ++[B](that: Args[B])(implicit zippable: Zippable[A, B]): Args[zippable.Out] =
    Args.Both(self, that).map { case (a, b) => zippable.zip(a, b) }

  /**
   * Repeats this argument one or more times and returns the values as a non-empty list. With no word to read, it fails
   * with the error of the repeated argument (`Missing argument <file>.`).
   */
  final def +[A1 >: A]: Args[::[A1]] = Args.Variadic(self, Some(1), None).map {
    case head :: tail => ::(head, tail)
    case Nil          => throw new IllegalStateException("Args.Variadic is not respecting the minimum.")
  }

  /**
   * Repeats this argument zero or more times and returns the values as a list. It reads values as long as words remain,
   * so an argument placed after it gets none (`Args.text.* ++ Args.text` fails with `Missing argument text.`), and a
   * word that doesn't parse makes the whole argument fail.
   */
  final def * : Args[List[A]] = Args.Variadic(self, None, None)

  /**
   * Returns a copy with `that` added as a paragraph to the description shown in help. On a combination (`++`, `map`, a
   * repetition), the paragraph is added to every argument inside; on `Args.none` it has no effect.
   */
  def ??(that: String): Args[A]

  /**
   * Repeats this argument at least `min` times, with no upper limit, and returns the values as a list. Fails when fewer
   * than `min` values are given.
   */
  final def atLeast(min: Int): Args[List[A]] = Args.Variadic(self, Some(min), None)

  /**
   * Repeats this argument at most `max` times and returns the values as a list; zero values are accepted. Words after
   * the first `max` values are left for what follows.
   */
  final def atMost(max: Int): Args[List[A]] = Args.Variadic(self, None, Some(max))

  /**
   * Repeats this argument at least `min` and at most `max` times and returns the values as a list. Words after the
   * first `max` values are left for what follows. When `min` is greater than `max`, it stops after `max` values without
   * failing.
   */
  final def between(min: Int, max: Int): Args[List[A]] = Args.Variadic(self, Some(min), Some(max))

  /**
   * Returns the help text of this argument, as shown in the arguments section of a command's help.
   */
  def helpDoc: HelpDoc

  /**
   * Transforms the parsed value with `f`.
   */
  final def map[B](f: A => B): Args[B] = Args.Map(self, (a: A) => Right(f(a)))

  /**
   * Transforms the parsed value with `f`. A `Left` makes parsing fail with that help text as the error message, as a
   * [[ValidationError]] of type `InvalidArgument`.
   */
  final def mapOrFail[B](f: A => Either[HelpDoc, B]): Args[B] =
    Args.Map(self, (a: A) => f(a))

  /**
   * Transforms the parsed value with `f`. An exception thrown by `f` makes parsing fail with the exception's message as
   * the error message. When that message is `null`, the error holds `null` text, and rendering it with
   * `HelpDoc#toPlaintext` throws a `java.lang.NullPointerException`.
   */
  final def mapTry[B](f: A => B): Args[B] =
    mapOrFail((a: A) => scala.util.Try(f(a)).toEither.left.map(e => HelpDoc.p(e.getMessage())))

  /**
   * Returns the largest number of words this argument can read. A repetition without an upper limit counts as
   * `Int.MaxValue / 2` times the size of the repeated argument, and that product overflows to a negative number when
   * the repeated argument reads three or more words.
   */
  def maxSize: Int

  /**
   * Returns the smallest number of words this argument needs.
   */
  def minSize: Int

  /**
   * Same as `*`: repeats this argument zero or more times.
   */
  final def repeat: Args[List[A]] = *

  /**
   * Same as `+`: repeats this argument one or more times.
   */
  final def repeat1[A1 >: A]: Args[::[A1]] = +

  /**
   * Returns the usage synopsis of this argument, such as `<file>` or `<file>...`, used in a command's usage line.
   */
  def synopsis: UsageSynopsis

  /**
   * Returns the names of the arguments inside, in angle brackets and joined with `, ` (`Some("<src>, <dst>")`), or
   * `None` for `Args.none`.
   */
  def uid: Option[String]

  /**
   * Parses this argument from the front of `args`, and returns the words it didn't read with the parsed value. Fails
   * with a [[ValidationError]] of type `InvalidArgument` when a word is missing or doesn't parse.
   */
  def validate(args: List[String], conf: CliConfig): IO[ValidationError, (List[String], A)]

  /**
   * The word `argument`, which the wizard mode uses to name this kind of parameter.
   */
  lazy val tag = "argument"

}

/**
 * Constructors for [[Args]]. Each value type has a method that takes the name shown in help, and most also have a `val`
 * that shows the type name instead (`Args.integer` shows as `<integer>`). A word that doesn't parse fails with
 * `<word> is not a <type>.`, such as `x is not a integer.`.
 */
object Args extends ArgsPlatformSpecific {

  /**
   * An argument that reads one word and parses it with `primType`. `pseudoName` is the name shown in angle brackets in
   * help and errors; when it is `None`, the type name is shown instead (`<text>`, `<integer>`).
   */
  final case class Single[+A](pseudoName: Option[String], primType: PrimType[A], description: HelpDoc = HelpDoc.Empty)
      extends Args[A]
      with Input {

    override lazy val shortDesc: String = s"Argument $name: ${description.getSpan.text}"

    /**
     * Returns a copy with `that` added as a paragraph to `description`.
     */
    def ??(that: String): Args[A] = copy(description = description + HelpDoc.p(that))

    /**
     * A description list with one entry: the name in angle brackets, followed by the type's description and
     * `description`.
     */
    lazy val helpDoc: HelpDoc =
      HelpDoc.DescriptionList(
        List(
          Span.weak(name) -> (p(primType.helpDoc) + description)
        )
      )

    /**
     * Always 1.
     */
    lazy val maxSize: Int = 1

    /**
     * Always 1.
     */
    lazy val minSize: Int = 1

    /**
     * The name in angle brackets, followed by the accepted words for an enumeration or a boolean (`<color> red |
     * blue`).
     */
    lazy val synopsis: UsageSynopsis = UsageSynopsis.Named(List(name), primType.choices)

    override def parse(args: List[String], conf: CliConfig): IO[ValidationError, (List[String], List[String])] =
      ZIO.succeed((Nil, Nil))

    /**
     * Parses the first word of `args` with `primType`. With no word left, it fails with `Missing argument <name>.`;
     * without a pseudo name, the type name appears without angle brackets (`Missing argument text.`), and an
     * enumeration or a boolean adds the accepted words (`Missing argument <color> with values red | blue.`).
     */
    def validate(args: List[String], conf: CliConfig): IO[ValidationError, (List[String], A)] =
      (args match {
        case head :: tail => primType.validate(Some(head), conf).mapBoth(text => HelpDoc.p(text), a => tail -> a)
        case Nil          =>
          val msg = (pseudoName, primType.choices) match {
            case (Some(pseudoName), Some(choices)) => s"Missing argument <$pseudoName> with values $choices."
            case (Some(pseudoName), _)             => s"Missing argument <$pseudoName>."
            case (None, Some(choices))             => s"Missing argument ${primType.typeName} with values $choices."
            case (None, None)                      => s"Missing argument ${primType.typeName}."
          }
          ZIO.fail(HelpDoc.p(msg))
      }).mapError(ValidationError(ValidationErrorType.InvalidArgument, _))

    private def name: String = "<" + pseudoName.getOrElse(primType.typeName) + ">"

    /**
     * The name in angle brackets, such as `Some("<file>")`.
     */
    def uid: Option[String] = Some(name)

    override def isValid(input: String, conf: CliConfig): IO[ValidationError, List[String]] =
      for {
        _ <- validate(List(input), conf)
      } yield List(input)
  }

  /**
   * The `Args` that reads no words and produces `()`; `Args.none` returns it.
   */
  case object Empty extends Args[Unit] with Pipeline {

    /**
     * Returns `Empty` unchanged: it has no help text to extend.
     */
    def ??(that: String): Args[Unit] = Empty

    /**
     * Always `HelpDoc.Empty`.
     */
    lazy val helpDoc: HelpDoc = HelpDoc.Empty

    /**
     * Always 0.
     */
    lazy val maxSize: Int = 0

    /**
     * Always 0.
     */
    lazy val minSize: Int = 0

    /**
     * Always `UsageSynopsis.None`.
     */
    lazy val synopsis: UsageSynopsis = UsageSynopsis.None

    /**
     * Succeeds with `()` and returns `args` unchanged.
     */
    def validate(args: List[String], conf: CliConfig): UIO[(List[String], Unit)] =
      ZIO.succeed((args, ()))

    /**
     * Always `None`.
     */
    def uid: Option[String] = None

    override def pipeline = ("", List())
  }

  /**
   * Two arguments parsed one after the other: `head`, then `tail` from the words `head` leaves. `++` builds it and
   * flattens the resulting tuple.
   */
  final case class Both[+A, +B](head: Args[A], tail: Args[B]) extends Args[(A, B)] with Pipeline {

    /**
     * Adds `that` to the descriptions of both `head` and `tail`.
     */
    def ??(that: String): Args[(A, B)] = Both(head ?? that, tail ?? that)

    /**
     * The help of `head` followed by the help of `tail`.
     */
    lazy val helpDoc: HelpDoc = head.helpDoc + tail.helpDoc

    /**
     * The sum of the `maxSize` of `head` and `tail`.
     */
    lazy val maxSize: Int = head.maxSize + tail.maxSize

    /**
     * The sum of the `minSize` of `head` and `tail`.
     */
    lazy val minSize: Int = head.minSize + tail.minSize

    /**
     * The synopsis of `head` followed by the synopsis of `tail`.
     */
    lazy val synopsis: UsageSynopsis = head.synopsis + tail.synopsis

    /**
     * Parses `head` from `args`, then `tail` from the words `head` leaves.
     */
    def validate(args: List[String], conf: CliConfig): IO[ValidationError, (List[String], (A, B))] =
      for {
        tuple    <- head.validate(args, conf)
        (args, a) = tuple
        tuple    <- tail.validate(args, conf)
        (args, b) = tuple
      } yield (args, (a, b))

    /**
     * The uids of `head` and `tail` joined with `, `, or `None` when both are `None`.
     */
    def uid: Option[String] = head.uid.toList ++ tail.uid.toList match {
      case Nil  => None
      case list => Some(list.mkString(", "))
    }

    override def pipeline = ("", List(head, tail))
  }

  /**
   * An argument repeated at least `min` and at most `max` times; `None` means no bound. `*`, `+`, `atLeast`, `atMost`
   * and `between` build it.
   */
  final case class Variadic[+A](value: Args[A], min: Option[Int], max: Option[Int]) extends Args[List[A]] with Input {

    override lazy val shortDesc: String = helpDoc.toPlaintext()

    /**
     * Adds `that` to the description of the repeated argument.
     */
    def ??(that: String): Args[List[A]] = Variadic(value ?? that, min, max)

    /**
     * The synopsis of the repeated argument followed by `...` (`<file>...`), whatever the bounds.
     */
    lazy val synopsis: UsageSynopsis = UsageSynopsis.Repeated(value.synopsis)

    /**
     * The help of the repeated argument, with the bounds after its name (`<file>...`, `<file> 1+` or `<file> 1 - 3`)
     * and a sentence on how often it may be repeated. When the repeated argument is a combination such as `(a ++ b).*`,
     * its help is returned unchanged.
     */
    lazy val helpDoc: HelpDoc = value.helpDoc.mapDescriptionList { case (span, block) =>
      val newSpan = span + Span.text(
        if (max.isDefined) s" $minSize - $maxSize" else if (minSize == 0) "..." else s" $minSize+"
      )
      val newBlock =
        block +
          HelpDoc.p(
            if (max.isDefined)
              s"This argument must be repeated at least $minSize times and may be repeated up to $maxSize times."
            else if (minSize == 0) "This argument may be repeated zero or more times."
            else s"This argument must be repeated at least $minSize times."
          )

      (newSpan, newBlock)
    }

    /**
     * `max` (or `Int.MaxValue / 2` when `max` is `None`) times the `maxSize` of the repeated argument. Without `max`,
     * the product overflows to a negative number when the repeated argument reads three or more words.
     */
    lazy val maxSize: Int = max.getOrElse(Int.MaxValue / 2) * value.maxSize

    /**
     * `min` (or 0 when `min` is `None`) times the `minSize` of the repeated argument.
     */
    lazy val minSize: Int = min.getOrElse(0) * value.minSize

    override def parse(args: List[String], conf: CliConfig): IO[ValidationError, (List[String], List[String])] =
      ZIO.succeed((Nil, Nil))

    /**
     * Parses the repeated argument again and again until `max` values are read or no word is left. Fails when fewer
     * than `min` values are read, or when a word fails to parse.
     */
    def validate(args: List[String], conf: CliConfig): IO[ValidationError, (List[String], List[A])] = {
      val min1 = min.getOrElse(0)
      val max1 = max.getOrElse(Int.MaxValue)

      def loop(args: List[String], acc: List[A]): IO[ValidationError, (List[String], List[A])] =
        if (acc.length >= max1) ZIO.succeed(args -> acc)
        else
          value
            .validate(args, conf)
            .foldZIO(
              failure => if (acc.length >= min1 && args.isEmpty) ZIO.succeed(args -> acc) else ZIO.fail(failure),
              { case (args, a) => loop(args, a :: acc) }
            )

      loop(args, Nil).map { case (args, list) => (args, list.reverse) }
    }

    /**
     * The uid of the repeated argument.
     */
    def uid: Option[String] = value.uid

    override def isValid(input: String, conf: CliConfig): IO[ValidationError, List[String]] =
      for {
        list <- ZIO.succeed(input.split(" ").toList)
        _    <- validate(list, conf)
      } yield list

  }

  /**
   * An argument whose parsed value is transformed by `f`; a `Left` from `f` makes parsing fail. `map`, `mapOrFail` and
   * `mapTry` build it.
   */
  final case class Map[A, B](value: Args[A], f: A => Either[HelpDoc, B]) extends Args[B] with Pipeline with Wrap {

    override lazy val shortDesc: String = value.shortDesc

    /**
     * Adds `that` to the description of `value`.
     */
    def ??(that: String): Args[B] = Map(value ?? that, f)

    /**
     * The help of `value`.
     */
    lazy val helpDoc: HelpDoc = value.helpDoc

    /**
     * The `maxSize` of `value`.
     */
    lazy val maxSize: Int = value.maxSize

    /**
     * The `minSize` of `value`.
     */
    lazy val minSize: Int = value.minSize

    /**
     * The synopsis of `value`.
     */
    lazy val synopsis: UsageSynopsis = value.synopsis

    /**
     * Parses `value`, then applies `f`. A `Left` fails with a [[ValidationError]] of type `InvalidArgument` that holds
     * the returned help text.
     */
    def validate(args: List[String], conf: CliConfig): IO[ValidationError, (List[String], B)] =
      value.validate(args, conf).flatMap { case (r, a) =>
        f(a) match {
          case Left(value)  => ZIO.fail(ValidationError(ValidationErrorType.InvalidArgument, value))
          case Right(value) => ZIO.succeed((r, value))
        }
      }

    /**
     * The uid of `value`.
     */
    def uid: Option[String] = value.uid

    override val wrapped = value

    override val pipeline = ("", List(value))
  }

  /**
   * Creates a boolean argument shown in help as `<name>`. It accepts `true`, `1`, `y`, `yes` and `on` for `true`, and
   * `false`, `0`, `n`, `no` and `off` for `false`; with the default case-insensitive [[CliConfig]], upper-case forms
   * such as `YES` are accepted too.
   *
   * @param name
   *   the name shown in help and error messages, in angle brackets
   */
  def bool(name: String): Args[Boolean] = Single(Some(name), PrimType.Bool(None))

  /**
   * A boolean argument shown in help as `<boolean>`. It accepts the same words as `bool(name)`.
   */
  val bool: Args[Boolean] = Single(None, PrimType.Bool(None))

  /**
   * Creates an argument whose value is one of `cases`, shown in help as `<name>` followed by the accepted words. Each
   * case pairs the word the user types with the value it produces; the word must match exactly, whatever the case
   * setting. Any other word fails with `Expected one of the following cases: ...`.
   *
   * {{{
   * val level: Args[Int] = Args.enumeration("level")("low" -> 1, "high" -> 2)
   * }}}
   *
   * @param name
   *   the name shown in help and error messages, in angle brackets
   * @param cases
   *   pairs of the word to type and the value it produces
   */
  def enumeration[A](name: String)(cases: (String, A)*): Args[A] =
    Single(Some(name), PrimType.Enumeration(cases: _*))

  /**
   * Creates an argument whose value is one of `cases`, shown in help as `<choice>` followed by the accepted words. Each
   * case pairs the word the user types with the value it produces.
   *
   * @param cases
   *   pairs of the word to type and the value it produces
   */
  def enumeration[A](cases: (String, A)*): Args[A] =
    Single(None, PrimType.Enumeration(cases: _*))

  /**
   * Creates an argument that accepts any word, shown in help as `<name>`.
   *
   * @param name
   *   the name shown in help and error messages, in angle brackets
   */
  def text(name: String): Args[String] =
    Single(Some(name), PrimType.Text)

  /**
   * An argument that accepts any word, shown in help as `<text>`.
   */
  val text: Args[String] =
    Single(None, PrimType.Text)

  /**
   * Creates an argument that parses a `BigDecimal`, such as `1.5`, shown in help as `<name>`.
   *
   * @param name
   *   the name shown in help and error messages, in angle brackets
   */
  def decimal(name: String): Args[BigDecimal] =
    Single(Some(name), PrimType.Decimal)

  /**
   * An argument that parses a `BigDecimal`, such as `1.5`, shown in help as `<decimal>`.
   */
  val decimal: Args[BigDecimal] =
    Single(None, PrimType.Decimal)

  /**
   * Creates an argument that parses a `java.time.Duration` in the ISO-8601 form, such as `PT1H` or `P1DT2H3M`, shown in
   * help as `<name>`.
   *
   * @param name
   *   the name shown in help and error messages, in angle brackets
   */
  def duration(name: String): Args[JDuration] =
    Single(Some(name), PrimType.Duration)

  /**
   * An argument that parses a `java.time.Duration` in the ISO-8601 form, such as `PT1H`, shown in help as `<duration>`.
   */
  val duration: Args[JDuration] =
    Single(None, PrimType.Duration)

  /**
   * Creates an argument that parses a `BigInt`, shown in help as `<name>`. A word such as `1.5` fails with
   * `1.5 is not a integer.`.
   *
   * @param name
   *   the name shown in help and error messages, in angle brackets
   */
  def integer(name: String): Args[BigInt] =
    Single(Some(name), PrimType.Integer)

  /**
   * An argument that parses a `BigInt`, shown in help as `<integer>`.
   */
  val integer: Args[BigInt] =
    Single(None, PrimType.Integer)

  /**
   * Creates an argument that parses a `java.time.Instant` in UTC, such as `2007-12-03T10:15:30.00Z`, shown in help as
   * `<name>`.
   *
   * @param name
   *   the name shown in help and error messages, in angle brackets
   */
  def instant(name: String): Args[JInstant] =
    Single(Some(name), PrimType.Instant)

  /**
   * An argument that parses a `java.time.Instant` in UTC, such as `2007-12-03T10:15:30.00Z`, shown in help as
   * `<instant>`.
   */
  val instant: Args[JInstant] =
    Single(None, PrimType.Instant)

  /**
   * Creates an argument that parses a `java.time.LocalDate`, such as `2007-12-03`, shown in help as `<name>`. There is
   * no `val` version of this constructor.
   *
   * @param name
   *   the name shown in help and error messages, in angle brackets
   */
  def localDate(name: String): Args[JLocalDate] =
    Single(Some(name), PrimType.LocalDate)

  /**
   * Creates an argument that parses a `java.time.LocalDateTime`, such as `2007-12-03T10:15:30`, shown in help as
   * `<name>`.
   *
   * @param name
   *   the name shown in help and error messages, in angle brackets
   */
  def localDateTime(name: String): Args[JLocalDateTime] =
    Single(Some(name), PrimType.LocalDateTime)

  /**
   * An argument that parses a `java.time.LocalDateTime`, such as `2007-12-03T10:15:30`, shown in help as `<date-time>`.
   */
  val localDateTime: Args[JLocalDateTime] =
    Single(None, PrimType.LocalDateTime)

  /**
   * Creates an argument that parses a `java.time.LocalTime`, such as `10:15:30`, shown in help as `<name>`.
   *
   * @param name
   *   the name shown in help and error messages, in angle brackets
   */
  def localTime(name: String): Args[JLocalTime] =
    Single(Some(name), PrimType.LocalTime)

  /**
   * An argument that parses a `java.time.LocalTime`, such as `10:15:30`, shown in help as `<local-time>`.
   */
  val localTime: Args[JLocalTime] =
    Single(None, PrimType.LocalTime)

  /**
   * Creates an argument that parses a `java.time.MonthDay` in the ISO-8601 form `--MM-DD`, such as `--12-03`, shown in
   * help as `<name>`. A word without the leading dashes, such as `12-03`, fails.
   *
   * @param name
   *   the name shown in help and error messages, in angle brackets
   */
  def monthDay(name: String): Args[JMonthDay] =
    Single(Some(name), PrimType.MonthDay)

  /**
   * An argument that parses a `java.time.MonthDay` in the ISO-8601 form `--MM-DD`, such as `--12-03`, shown in help as
   * `<month-day>`.
   */
  val monthDay: Args[JMonthDay] =
    Single(None, PrimType.MonthDay)

  /**
   * An `Args` that reads no words and produces `()`. Use it for a command without arguments; `++` drops its `Unit`
   * value.
   */
  val none: Args[Unit] = Empty

  /**
   * Creates an argument that parses a `java.time.OffsetDateTime`, such as `2007-12-03T10:15:30+01:00`, shown in help as
   * `<name>`.
   *
   * @param name
   *   the name shown in help and error messages, in angle brackets
   */
  def offsetDateTime(name: String): Args[JOffsetDateTime] =
    Single(Some(name), PrimType.OffsetDateTime)

  /**
   * An argument that parses a `java.time.OffsetDateTime`, such as `2007-12-03T10:15:30+01:00`, shown in help as
   * `<offset-date-time>`.
   */
  val offsetDateTime: Args[JOffsetDateTime] =
    Single(None, PrimType.OffsetDateTime)

  /**
   * Creates an argument that parses a `java.time.OffsetTime`, such as `10:15:30+01:00`, shown in help as `<name>`.
   *
   * @param name
   *   the name shown in help and error messages, in angle brackets
   */
  def offsetTime(name: String): Args[JOffsetTime] =
    Single(Some(name), PrimType.OffsetTime)

  /**
   * An argument that parses a `java.time.OffsetTime`, such as `10:15:30+01:00`, shown in help as `<offset-time>`.
   */
  val offsetTime: Args[JOffsetTime] =
    Single(None, PrimType.OffsetTime)

  /**
   * Creates an argument that parses a `java.time.Period` in the ISO-8601 form, such as `P1Y2M3D`, shown in help as
   * `<name>`.
   *
   * @param name
   *   the name shown in help and error messages, in angle brackets
   */
  def period(name: String): Args[JPeriod] =
    Single(Some(name), PrimType.Period)

  /**
   * An argument that parses a `java.time.Period` in the ISO-8601 form, such as `P1Y2M3D`, shown in help as `<period>`.
   */
  val period: Args[JPeriod] =
    Single(None, PrimType.Period)

  /**
   * Creates an argument that parses a `java.time.Year`, such as `2007`, shown in help as `<name>`.
   *
   * @param name
   *   the name shown in help and error messages, in angle brackets
   */
  def year(name: String): Args[JYear] =
    Single(Some(name), PrimType.Year)

  /**
   * An argument that parses a `java.time.Year`, such as `2007`, shown in help as `<year>`.
   */
  val year: Args[JYear] =
    Single(None, PrimType.Year)

  /**
   * Creates an argument that parses a `java.time.YearMonth`, such as `2007-12`, shown in help as `<name>`.
   *
   * @param name
   *   the name shown in help and error messages, in angle brackets
   */
  def yearMonth(name: String): Args[JYearMonth] =
    Single(Some(name), PrimType.YearMonth)

  /**
   * An argument that parses a `java.time.YearMonth`, such as `2007-12`, shown in help as `<year-month>`.
   */
  val yearMonth: Args[JYearMonth] =
    Single(None, PrimType.YearMonth)

  /**
   * Creates an argument that parses a `java.time.ZonedDateTime`, such as `2007-12-03T10:15:30+01:00[Europe/Paris]`,
   * shown in help as `<name>`. The zone ID goes in square brackets right after the offset.
   *
   * @param name
   *   the name shown in help and error messages, in angle brackets
   */
  def zonedDateTime(name: String): Args[JZonedDateTime] =
    Single(Some(name), PrimType.ZonedDateTime)

  /**
   * An argument that parses a `java.time.ZonedDateTime`, such as `2007-12-03T10:15:30+01:00[Europe/Paris]`, shown in
   * help as `<zoned-date-time>`.
   */
  val zonedDateTime: Args[JZonedDateTime] =
    Single(None, PrimType.ZonedDateTime)

  /**
   * Creates an argument that parses a `java.time.ZoneId`, such as `Europe/Paris`, shown in help as `<name>`.
   *
   * @param name
   *   the name shown in help and error messages, in angle brackets
   */
  def zoneId(name: String): Args[JZoneId] =
    Single(Some(name), PrimType.ZoneId)

  /**
   * An argument that parses a `java.time.ZoneId`, such as `Europe/Paris`, shown in help as `<zone-id>`.
   */
  val zoneId: Args[JZoneId] =
    Single(None, PrimType.ZoneId)

  /**
   * Creates an argument that parses a `java.time.ZoneOffset`, such as `+02:00`, shown in help as `<name>`.
   *
   * @param name
   *   the name shown in help and error messages, in angle brackets
   */
  def zoneOffset(name: String): Args[JZoneOffset] =
    Single(Some(name), PrimType.ZoneOffset)

  /**
   * An argument that parses a `java.time.ZoneOffset`, such as `+02:00`, shown in help as `<zone-offset>`.
   */
  val zoneOffset: Args[JZoneOffset] =
    Single(None, PrimType.ZoneOffset)
}
