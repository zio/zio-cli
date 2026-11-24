package zio.cli

import zio._
import zio.cli.HelpDoc.Span.text
import zio.cli.files.FileSystem

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
 * A `PrimType` represents the primitive types supported by ZIO CLI: one kind of value that an option or argument reads
 * from the command line. It gives the placeholder name and the help sentence for the value, and parses a string into an
 * `A`.
 *
 * Constructors such as `Args.integer` and `Options.file` wrap a `PrimType`, and `Args.Single` and `Options.Single`
 * accept any `PrimType`. `validate` parses a string without building a command, for example in a test:
 *
 * {{{
 * import zio._
 *
 * val count: IO[String, BigInt] = PrimType.Integer.validate("42")
 * // succeeds with 42
 * val wrong: IO[String, BigInt] = PrimType.Integer.validate("ten")
 * // fails with "ten is not a integer."
 * }}}
 *
 * @see
 *   [[https://zio.dev/zio-cli/options/ Options]]
 * @see
 *   [[https://zio.dev/zio-cli/args/ Args]]
 */
sealed trait PrimType[+A] { self =>

  /**
   * Returns the sentence that the help shows for a value of this type, such as "An integer." for `Integer`.
   */
  def helpDoc: HelpDoc.Span

  /**
   * Returns `true` for `Bool` and `false` for the other types. A boolean option may appear without a value, and its
   * usage line shows no placeholder.
   */
  def isBool: Boolean

  /**
   * Returns the placeholder name of this type, such as `integer` or `date`. Help and usage lines show it in angle
   * brackets, as in `--count <integer>` or `<integer>`, for an option or argument that has no pseudo-name; an
   * enumeration option shows its words instead (`--color red | green`), and a boolean option shows no placeholder.
   */
  def typeName: String

  /**
   * Returns the accepted words joined with ` | `, or `None` when the type has no fixed list. `Bool` gives
   * `true | false` and `Enumeration` gives its case words; usage lines show this text after an argument's placeholder
   * (`<choice> red | green`) and in place of an enumeration option's placeholder (`--color red | green`); a boolean
   * option shows neither.
   */
  def choices: Option[String]

  /**
   * Parses `value` with `CliConfig.default`. A `null` value is treated as a missing one.
   */
  final def validate(value: String): IO[String, A] = validate(Option(value), CliConfig.default)

  /**
   * Parses `value` with the given config. A `null` value is treated as a missing one.
   */
  final def validate(value: String, conf: CliConfig): IO[String, A] = validate(Option(value), conf)

  /**
   * Parses `value`, or handles a missing value (`None`). The error is a message for the user, such as
   * `ten is not a integer.`.
   *
   * Every type except `Bool` fails on `None`; `Bool` returns its default value if it has one. Only `Bool` reads `conf`,
   * to decide whether the case of the input matters.
   */
  def validate(value: Option[String], conf: CliConfig): IO[String, A]
}

/**
 * The value types of ZIO CLI: text, numbers, booleans, enumerations, paths and the `java.time` types.
 */
object PrimType extends PathPlatformSpecific {

  /**
   * A file-system path, checked for its kind and its existence. `validate` parses the string with `fileSystem` and
   * returns the parsed path. It first applies the existence rule of `shouldExist`; then, if the path exists and
   * `shouldExist` is not `Exists.No`, the kind rule of `pathType`. A missing path accepted by `Exists.Either` gets no
   * kind check.
   *
   * On Scala.js, `FileSystem.live` can't parse paths, so validation with it fails.
   *
   * @param pathType
   *   The kind of path expected: `PathType.File`, `PathType.Directory`, or `PathType.Either` when both are acceptable.
   * @param shouldExist
   *   Whether the path must exist (`Exists.Yes`), must not exist (`Exists.No`), or may do either (`Exists.Either`).
   * @param fileSystem
   *   The file system that parses and checks the path; `FileSystem.live` uses the real one.
   */
  final case class Path(pathType: PathType, shouldExist: Exists, fileSystem: FileSystem = FileSystem.live)
      extends PrimType[JPath] { self =>
    import PathType._

    /**
     * Returns a sentence built from `pathType` and `shouldExist`, such as "An existing file.", "A directory that must
     * not exist." or "A file or directory.".
     */
    lazy val helpDoc: HelpDoc.Span = (self.pathType, self.shouldExist) match {
      case (PathType.Either, Exists.Yes)       => text("An existing file or directory.")
      case (PathType.File, Exists.Yes)         => text("An existing file.")
      case (PathType.Directory, Exists.Yes)    => text("An existing directory.")
      case (PathType.Either, Exists.No)        => text("A file or directory that must not exist.")
      case (PathType.File, Exists.No)          => text("A file that must not exist.")
      case (PathType.Directory, Exists.No)     => text("A directory that must not exist.")
      case (PathType.Either, Exists.Either)    => text("A file or directory.")
      case (PathType.File, Exists.Either)      => text("A file.")
      case (PathType.Directory, Exists.Either) => text("A directory.")
    }

    /** Returns `false`. */
    lazy val isBool: Boolean = false

    /**
     * Returns `file`, `directory`, or `path` for `PathType.Either`.
     */
    lazy val typeName: String =
      self.pathType match {
        case PathType.Either    => "path"
        case PathType.File      => "file"
        case PathType.Directory => "directory"
      }

    /** Returns `None`. */
    lazy val choices: Option[String] = None

    /**
     * Parses and checks the path. For the input `out.txt`, the existence rule fails with `Path 'out.txt' must exist.`
     * (`Exists.Yes`) or `Path 'out.txt' must not exist.` (`Exists.No`), and the kind rule fails with
     * `Expected path 'out.txt' to be a regular file.` (`PathType.File`) or `Expected path 'out.txt' to be a directory.`
     * (`PathType.Directory`).
     *
     * A string that is not a path fails with the error of `fileSystem.parsePath`; on the JVM, `FileSystem.live` gives
     * `'out.txt' is not a recognized path.` with the input in place of `out.txt`. A missing value fails with
     * `Path options do not have a default value`. `conf` is not used.
     */
    def validate(value: Option[String], conf: CliConfig): IO[String, JPath] =
      for {
        value  <- ZIO.fromOption(value).orElseFail("Path options do not have a default value")
        path   <- self.fileSystem.parsePath(value)
        exists <- self.fileSystem.exists(path)
        _      <- validateExistence(value, self.shouldExist, exists)
        _      <- validatePathType(value, path).when(self.shouldExist != Exists.No && exists)
      } yield path

    private def validatePathType(value: String, path: JPath): IO[String, Unit] =
      self.pathType match {
        case Either => ZIO.unit
        case File   =>
          ZIO.fail(s"Expected path '$value' to be a regular file.").unlessZIO(self.fileSystem.isRegularFile(path)).unit
        case Directory =>
          ZIO.fail(s"Expected path '$value' to be a directory.").unlessZIO(self.fileSystem.isDirectory(path)).unit
      }

    private def validateExistence(path: String, expected: Exists, actual: Boolean): IO[String, Unit] =
      (expected, actual) match {
        case (Exists.No, true)   => ZIO.fail(s"Path '$path' must not exist.")
        case (Exists.Yes, false) => ZIO.fail(s"Path '$path' must exist.")
        case _                   => ZIO.unit
      }
  }

  /**
   * A value chosen from a fixed list of words. Each case pairs the word the user types with the value it stands for.
   *
   * The input must equal a word exactly, including its case, even when the `CliConfig` is case-insensitive. When two
   * cases have the same word, the first one wins.
   *
   * @param cases
   *   The pairs of word and value, in the order the help lists them.
   */
  final case class Enumeration[A](cases: (String, A)*) extends PrimType[A] { self =>

    /**
     * Returns "One of the following cases: " followed by the words joined with `, ` and a period, such as "One of the
     * following cases: red, green.".
     */
    lazy val helpDoc: HelpDoc.Span = text("One of the following cases: " + self.cases.map(_._1).mkString(", ") + ".")

    /** Returns `false`. */
    lazy val isBool: Boolean = false

    /** Returns `choice`. */
    lazy val typeName: String = "choice"

    /**
     * Returns the words joined with ` | `, such as `Some("red | green")`.
     */
    lazy val choices: Option[String] = Some(self.cases.map(_._1).mkString(" | "))

    /**
     * Returns the value of the first case whose word equals the input. Any other input fails with
     * `Expected one of the following cases: red, green` (the words of this enumeration, with no final period), and a
     * missing value fails with `Enumeration options do not have a default value.`. `conf` is not used.
     */
    def validate(value: Option[String], conf: CliConfig): IO[String, A] =
      (ZIO.fromOption(value) orElseFail "Enumeration options do not have a default value.").flatMap { value =>
        self.cases.find(_._1 == value) match {
          case None         => ZIO.fail("Expected one of the following cases: " + self.cases.map(_._1).mkString(", "))
          case Some((_, a)) => ZIO.succeed(a)
        }
      }
  }

  /**
   * Type representing any text. It accepts every string, including an empty one, and returns it unchanged.
   */
  case object Text extends PrimType[String] { self =>

    /** Returns `false`. */
    lazy val isBool: Boolean = false

    /** Returns `text`. */
    lazy val typeName: String = "text"

    /** Returns `None`. */
    lazy val choices: Option[String] = None

    /**
     * Returns the input unchanged. Fails only on a missing value, with `text options do not have a default value.`.
     */
    def validate(value: Option[String], conf: CliConfig): IO[String, String] =
      attempt(value, v => v, self.typeName)

    /** Returns "A user-defined piece of text.". */
    lazy val helpDoc: HelpDoc.Span = text("A user-defined piece of text.")
  }

  /**
   * Type representing decimal value via BigDecimal. The input is read with `BigDecimal(...)`, so `1.5`, `-2` and `1e3`
   * parse, while ` 1`, with a leading space, doesn't.
   */
  case object Decimal extends PrimType[BigDecimal] { self =>

    /** Returns `false`. */
    lazy val isBool: Boolean = false

    /** Returns `decimal`. */
    lazy val typeName: String = "decimal"

    /** Returns `None`. */
    lazy val choices: Option[String] = None

    /**
     * Parses the input with `BigDecimal(...)`. Fails with `abc is not a decimal.` for the input `abc`, and with
     * `decimal options do not have a default value.` when the value is missing.
     */
    def validate(value: Option[String], conf: CliConfig): IO[String, BigDecimal] =
      attempt(value, BigDecimal(_), self.typeName)

    /** Returns "A decimal number.". */
    lazy val helpDoc: HelpDoc.Span = text("A decimal number.")
  }

  /**
   * Type representing integer value via BigInt. The input is read with `BigInt(...)`, so numbers larger than a `Long`,
   * such as `123456789012345678901234567890`, parse, with an optional `+` or `-` sign; `1.5` and `0x10` don't.
   */
  case object Integer extends PrimType[BigInt] { self =>

    /** Returns `false`. */
    lazy val isBool: Boolean = false

    /** Returns `integer`. */
    lazy val typeName: String = "integer"

    /** Returns `None`. */
    lazy val choices: Option[String] = None

    /**
     * Parses the input with `BigInt(...)`. Fails with `ten is not a integer.` for the input `ten`, and with
     * `integer options do not have a default value.` when the value is missing.
     */
    def validate(value: Option[String], conf: CliConfig): IO[String, BigInt] =
      attempt(value, BigInt(_), self.typeName)

    /** Returns "An integer.". */
    lazy val helpDoc: HelpDoc.Span = text("An integer.")
  }

  /**
   * Type representing a boolean value. True value can be passed as "true", "1", "y", "yes" or "on". False value can be
   * passed as "false", "0", "n", "no" or "off".
   *
   * The words are compared after lowercasing the input, unless the `CliConfig` is case-sensitive: with
   * `CliConfig.default`, `TRUE` and `Yes` parse, while a case-sensitive config rejects them.
   *
   * @param defaultValue
   *   The value that `validate` returns when the value is missing (`None`), as for a boolean flag given without a
   *   value.
   */
  final case class Bool(defaultValue: Option[Boolean]) extends PrimType[Boolean] { self =>

    /** Returns `true`. */
    lazy val isBool: Boolean = true

    /** Returns `boolean`. */
    lazy val typeName: String = "boolean"

    /** Returns `Some("true | false")`. */
    lazy val choices: Option[String] = Some("true | false")

    /**
     * Returns `true` for a word in `Bool.TrueValues` and `false` for a word in `Bool.FalseValues`, after lowercasing
     * the input unless `conf` is case-sensitive.
     *
     * Any other input fails with `maybe cannot be recognized as valid boolean.` (for the input `maybe`; the message
     * shows the lowercased input when `conf` is case-insensitive). A missing value returns `defaultValue`, or fails
     * with `Missing default for bool parameter.` when there is none.
     */
    def validate(value: Option[String], conf: CliConfig): IO[String, Boolean] =
      value.map(conf.normalizeCase) match {
        case Some(s) if Bool.TrueValues(s)  => ZIO.succeed(true)
        case Some(s) if Bool.FalseValues(s) => ZIO.succeed(false)
        case Some(s)                        => ZIO.fail(s"$s cannot be recognized as valid boolean.")
        case None                           => ZIO.fromOption(self.defaultValue).orElseFail("Missing default for bool parameter.")
      }

    /** Returns "A true or false value.". */
    lazy val helpDoc: HelpDoc.Span = text("A true or false value.")
  }

  /**
   * The words that `Bool` accepts.
   */
  object Bool {

    /** The lowercase words read as `true`: `true`, `1`, `y`, `yes` and `on`. */
    lazy val TrueValues: Set[String] = Set("true", "1", "y", "yes", "on")

    /** The lowercase words read as `false`: `false`, `0`, `n`, `no` and `off`. */
    lazy val FalseValues: Set[String] = Set("false", "0", "n", "no", "off")
  }

  /**
   * Type representing a time-based amount of time in the ISO-8601 format, such as 'P1DT2H3M'. The input is read with
   * `java.time.Duration.parse`, so `PT1H30M` parses too.
   */
  case object Duration extends PrimType[JDuration] { self =>

    /** Returns `false`. */
    lazy val isBool: Boolean = false

    /** Returns `duration`. */
    lazy val typeName: String = "duration"

    /** Returns `None`. */
    lazy val choices: Option[String] = None

    /**
     * Parses the input with `java.time.Duration.parse`. Fails with `1h is not a duration.` for the input `1h`, and with
     * `duration options do not have a default value.` when the value is missing.
     */
    def validate(value: Option[String], conf: CliConfig): IO[String, JDuration] =
      attempt(value, JDuration.parse, self.typeName)

    /**
     * Returns "A time-based amount of time in the ISO-8601 format, such as 'P1DT2H3M'.".
     */
    lazy val helpDoc: HelpDoc.Span =
      text("A time-based amount of time in the ISO-8601 format, such as 'P1DT2H3M'.")
  }

  /**
   * Type representing parameter for instant in time in UTC format, such as 2007-12-03T10:15:30.00Z. The input is read
   * with `java.time.Instant.parse`.
   */
  case object Instant extends PrimType[JInstant] { self =>

    /** Returns `false`. */
    lazy val isBool: Boolean = false

    /** Returns `instant`. */
    lazy val typeName: String = "instant"

    /** Returns `None`. */
    lazy val choices: Option[String] = None

    /**
     * Parses the input with `java.time.Instant.parse`. Fails with `2007-12-03 is not a instant.` for the input
     * `2007-12-03`, and with `instant options do not have a default value.` when the value is missing.
     */
    def validate(value: Option[String], conf: CliConfig): IO[String, JInstant] =
      attempt(value, JInstant.parse, self.typeName)

    /**
     * Returns "An instant in time in UTC format, such as 2007-12-03T10:15:30.00Z.".
     */
    lazy val helpDoc: HelpDoc.Span = text("An instant in time in UTC format, such as 2007-12-03T10:15:30.00Z.")
  }

  /**
   * Type representing parameter for a date in ISO_LOCAL_DATE format, such as 2007-12-03. The input is read with
   * `java.time.LocalDate.parse`, so `2007-12-3` doesn't parse.
   */
  case object LocalDate extends PrimType[JLocalDate] { self =>

    /** Returns `false`. */
    lazy val isBool: Boolean = false

    /** Returns `date`. */
    lazy val typeName: String = "date"

    /** Returns `None`. */
    lazy val choices: Option[String] = None

    /**
     * Parses the input with `java.time.LocalDate.parse`. Fails with `2007-12-3 is not a date.` for the input
     * `2007-12-3`, and with `date options do not have a default value.` when the value is missing.
     */
    def validate(value: Option[String], conf: CliConfig): IO[String, JLocalDate] =
      attempt(value, JLocalDate.parse, self.typeName)

    /**
     * Returns "A date in ISO_LOCAL_DATE format, such as 2007-12-03" (with no final period).
     */
    lazy val helpDoc: HelpDoc.Span = text("A date in ISO_LOCAL_DATE format, such as 2007-12-03")
  }

  /**
   * Type representing a date-time without a time-zone in the ISO-8601 format, such as 2007-12-03T10:15:30. The input is
   * read with `java.time.LocalDateTime.parse`, so `2007-12-03T10:15` parses too.
   */
  case object LocalDateTime extends PrimType[JLocalDateTime] { self =>

    /** Returns `false`. */
    lazy val isBool: Boolean = false

    /** Returns `date-time`. */
    lazy val typeName: String = "date-time"

    /** Returns `None`. */
    lazy val choices: Option[String] = None

    /**
     * Parses the input with `java.time.LocalDateTime.parse`. Fails with `2007-12-03 is not a date-time.` for the input
     * `2007-12-03`, and with `date-time options do not have a default value.` when the value is missing.
     */
    def validate(value: Option[String], conf: CliConfig): IO[String, JLocalDateTime] =
      attempt(value, JLocalDateTime.parse, self.typeName)

    /**
     * Returns "A date-time without a time-zone in the ISO-8601 format, such as 2007-12-03T10:15:30.".
     */
    lazy val helpDoc: HelpDoc.Span =
      text("A date-time without a time-zone in the ISO-8601 format, such as 2007-12-03T10:15:30.")
  }

  /**
   * Type representing a time without a time-zone in the ISO-8601 format, such as 10:15:30. The input is read with
   * `java.time.LocalTime.parse`, so `10:15` parses too.
   */
  case object LocalTime extends PrimType[JLocalTime] { self =>

    /** Returns `false`. */
    lazy val isBool: Boolean = false

    /** Returns `local-time`. */
    lazy val typeName: String = "local-time"

    /** Returns `None`. */
    lazy val choices: Option[String] = None

    /**
     * Parses the input with `java.time.LocalTime.parse`. Fails with `10 is not a local-time.` for the input `10`, and
     * with `local-time options do not have a default value.` when the value is missing.
     */
    def validate(value: Option[String], conf: CliConfig): IO[String, JLocalTime] =
      attempt(value, JLocalTime.parse, self.typeName)

    /**
     * Returns "A time without a time-zone in the ISO-8601 format, such as 10:15:30.".
     */
    lazy val helpDoc: HelpDoc.Span = text("A time without a time-zone in the ISO-8601 format, such as 10:15:30.")
  }

  /**
   * Type representing a month-day in the ISO-8601 format, such as --12-03. The input is read with
   * `java.time.MonthDay.parse`, which needs the two leading dashes: `--12-03` parses, while `12-03`, the example in the
   * help sentence, doesn't.
   */
  case object MonthDay extends PrimType[JMonthDay] { self =>

    /** Returns `false`. */
    lazy val isBool: Boolean = false

    /** Returns `month-day`. */
    lazy val typeName: String = "month-day"

    /** Returns `None`. */
    lazy val choices: Option[String] = None

    /**
     * Parses the input with `java.time.MonthDay.parse`. Fails with `12-03 is not a month-day.` for the input `12-03`,
     * and with `month-day options do not have a default value.` when the value is missing.
     */
    def validate(value: Option[String], conf: CliConfig): IO[String, JMonthDay] =
      attempt(value, JMonthDay.parse, self.typeName)

    /**
     * Returns "A month-day in the ISO-8601 format such as 12-03.".
     */
    lazy val helpDoc: HelpDoc.Span = text("A month-day in the ISO-8601 format such as 12-03.")
  }

  /**
   * Type representing a date-time with an offset from UTC/Greenwich in the ISO-8601 format, such as
   * 2007-12-03T10:15:30+01:00. The input is read with `java.time.OffsetDateTime.parse`, so `Z` works as the offset too.
   */
  case object OffsetDateTime extends PrimType[JOffsetDateTime] { self =>

    /** Returns `false`. */
    lazy val isBool: Boolean = false

    /** Returns `offset-date-time`. */
    lazy val typeName: String = "offset-date-time"

    /** Returns `None`. */
    lazy val choices: Option[String] = None

    /**
     * Parses the input with `java.time.OffsetDateTime.parse`. Fails with
     * `2007-12-03T10:15:30 is not a offset-date-time.` for an input without an offset, and with
     * `offset-date-time options do not have a default value.` when the value is missing.
     */
    def validate(value: Option[String], conf: CliConfig): IO[String, JOffsetDateTime] =
      attempt(value, JOffsetDateTime.parse, self.typeName)

    /**
     * Returns "A date-time with an offset from UTC/Greenwich in the ISO-8601 format, such as
     * 2007-12-03T10:15:30+01:00.".
     */
    lazy val helpDoc: HelpDoc.Span =
      text("A date-time with an offset from UTC/Greenwich in the ISO-8601 format, such as 2007-12-03T10:15:30+01:00.")
  }

  /**
   * Type representing a time with an offset from UTC/Greenwich in the ISO-8601 format, such as 10:15:30+01:00. The
   * input is read with `java.time.OffsetTime.parse`.
   */
  case object OffsetTime extends PrimType[JOffsetTime] { self =>

    /** Returns `false`. */
    lazy val isBool: Boolean = false

    /** Returns `offset-time`. */
    lazy val typeName: String = "offset-time"

    /** Returns `None`. */
    lazy val choices: Option[String] = None

    /**
     * Parses the input with `java.time.OffsetTime.parse`. Fails with `10:15:30 is not a offset-time.` for an input
     * without an offset, and with `offset-time options do not have a default value.` when the value is missing.
     */
    def validate(value: Option[String], conf: CliConfig): IO[String, JOffsetTime] =
      attempt(value, JOffsetTime.parse, self.typeName)

    /**
     * Returns "A time with an offset from UTC/Greenwich in the ISO-8601 format, such as 10:15:30+01:00.".
     */
    lazy val helpDoc: HelpDoc.Span =
      text("A time with an offset from UTC/Greenwich in the ISO-8601 format, such as 10:15:30+01:00.")
  }

  /**
   * Type representing a date-based amount of time in the ISO-8601 format, such as 'P1Y2M3D'. The input is read with
   * `java.time.Period.parse`, so `P2W` parses too, while a time part such as `PT1H` doesn't.
   */
  case object Period extends PrimType[JPeriod] { self =>

    /** Returns `false`. */
    lazy val isBool: Boolean = false

    /** Returns `period`. */
    lazy val typeName: String = "period"

    /** Returns `None`. */
    lazy val choices: Option[String] = None

    /**
     * Parses the input with `java.time.Period.parse`. Fails with `PT1H is not a period.` for the input `PT1H`, and with
     * `period options do not have a default value.` when the value is missing.
     */
    def validate(value: Option[String], conf: CliConfig): IO[String, JPeriod] =
      attempt(value, JPeriod.parse, self.typeName)

    /**
     * Returns "A date-based amount of time in the ISO-8601 format, such as 'P1Y2M3D'.".
     */
    lazy val helpDoc: HelpDoc.Span =
      text("A date-based amount of time in the ISO-8601 format, such as 'P1Y2M3D'.")
  }

  /**
   * Type representing a year, such as 2007. The input is read as an `Int` and passed to `java.time.Year.of`, so `07`
   * gives the year 7, `-5` and `+2007` parse, and `1000000000` fails.
   */
  case object Year extends PrimType[JYear] { self =>

    /** Returns `false`. */
    lazy val isBool: Boolean = false

    /** Returns `year`. */
    lazy val typeName: String = "year"

    /** Returns `None`. */
    lazy val choices: Option[String] = None

    /**
     * Parses the input as an `Int` and builds the year with `java.time.Year.of`. Fails with `abc is not a year.` for
     * the input `abc`, and with `year options do not have a default value.` when the value is missing.
     */
    def validate(value: Option[String], conf: CliConfig): IO[String, JYear] =
      attempt(value, s => JYear.of(s.toInt), self.typeName)

    /** Returns "A year in the ISO-8601 format, such as 2007.". */
    lazy val helpDoc: HelpDoc.Span = text("A year in the ISO-8601 format, such as 2007.")
  }

  /**
   * Type representing a year-month in the ISO-8601 format, such as 2007-12. The input is an optional `-`, the digits of
   * the year, a `-` and a two-digit month from `01` to `12`; `07-12` gives the year 7, while `+2007-12` and `2007-1`
   * fail.
   */
  case object YearMonth extends PrimType[JYearMonth] { self =>

    /** Returns `false`. */
    lazy val isBool: Boolean = false

    /** Returns `year-month`. */
    lazy val typeName: String = "year-month"

    /** Returns `None`. */
    lazy val choices: Option[String] = None

    /**
     * Parses the input and builds the value with `java.time.YearMonth.of`. Fails with `2007-13 is not a year-month.`
     * for the input `2007-13`. A missing value fails with ` is not a year-month.`, which starts with a space because
     * the input is empty.
     */
    def validate(value: Option[String], conf: CliConfig): IO[String, JYearMonth] = {
      val AcceptedFormat       = "^(-?\\d+)-(\\d{2})".r
      def parse(input: String) = input match {
        case AcceptedFormat(y, m) => ZIO.attempt(JYearMonth.of(y.toInt, m.toInt))
        case _                    => ZIO.fail(())
      }

      (ZIO.fromOption(value) orElseFail "year-month does not have a default value").flatMap(parse) orElseFail
        s"${value.getOrElse("")} is not a ${self.typeName}."
    }

    /** Returns "A year-month in the ISO-8601 format, such as 2007-12.". */
    lazy val helpDoc: HelpDoc.Span = text("A year-month in the ISO-8601 format, such as 2007-12.")
  }

  /**
   * Type representing a date-time with a time-zone in the ISO-8601 format, such as
   * 2007-12-03T10:15:30+01:00[Europe/Paris]. The input is read with `java.time.ZonedDateTime.parse`, which needs the
   * zone ID in square brackets; an offset alone, as in `2007-12-03T10:15:30+01:00`, parses too. The help sentence shows
   * the zone after a space, and that form doesn't parse.
   */
  case object ZonedDateTime extends PrimType[JZonedDateTime] { self =>

    /** Returns `false`. */
    lazy val isBool: Boolean = false

    /** Returns `zoned-date-time`. */
    lazy val typeName: String = "zoned-date-time"

    /** Returns `None`. */
    lazy val choices: Option[String] = None

    /**
     * Parses the input with `java.time.ZonedDateTime.parse`. Fails with `2007-12-03T10:15:30 is not a zoned-date-time.`
     * for an input without a zone, and with `zoned-date-time options do not have a default value.` when the value is
     * missing.
     */
    def validate(value: Option[String], conf: CliConfig): IO[String, JZonedDateTime] =
      attempt(value, JZonedDateTime.parse, self.typeName)

    /**
     * Returns "A date-time with a time-zone in the ISO-8601 format, such as 2007-12-03T10:15:30+01:00 Europe/Paris.".
     */
    lazy val helpDoc: HelpDoc.Span =
      text("A date-time with a time-zone in the ISO-8601 format, such as 2007-12-03T10:15:30+01:00 Europe/Paris.")
  }

  /**
   * Type representing a time-zone ID, such as Europe/Paris. The input is read with `java.time.ZoneId.of`, so `UTC` and
   * offsets such as `+02:00` parse too.
   */
  case object ZoneId extends PrimType[JZoneId] { self =>

    /** Returns `false`. */
    lazy val isBool: Boolean = false

    /** Returns `zone-id`. */
    lazy val typeName: String = "zone-id"

    /** Returns `None`. */
    lazy val choices: Option[String] = None

    /**
     * Parses the input with `java.time.ZoneId.of`. Fails with `Mars/Base is not a zone-id.` for the input `Mars/Base`,
     * and with `zone-id options do not have a default value.` when the value is missing.
     */
    def validate(value: Option[String], conf: CliConfig): IO[String, JZoneId] =
      attempt(value, JZoneId.of, self.typeName)

    /** Returns "A time-zone ID, such as Europe/Paris.". */
    lazy val helpDoc: HelpDoc.Span = text("A time-zone ID, such as Europe/Paris.")
  }

  /**
   * Type representing a time-zone offset from Greenwich/UTC, such as +02:00. The input is read with
   * `java.time.ZoneOffset.of`, so `Z`, `+2` and `-0530` parse too, while `+19:00` fails.
   */
  case object ZoneOffset extends PrimType[JZoneOffset] { self =>

    /** Returns `false`. */
    lazy val isBool: Boolean = false

    /** Returns `zone-offset`. */
    lazy val typeName: String = "zone-offset"

    /** Returns `None`. */
    lazy val choices: Option[String] = None

    /**
     * Parses the input with `java.time.ZoneOffset.of`. Fails with `+19:00 is not a zone-offset.` for the input
     * `+19:00`, and with `zone-offset options do not have a default value.` when the value is missing.
     */
    def validate(value: Option[String], conf: CliConfig): IO[String, JZoneOffset] =
      attempt(value, JZoneOffset.of, typeName)

    /** Returns "A time-zone offset from Greenwich/UTC, such as +02:00.". */
    lazy val helpDoc: HelpDoc.Span = text("A time-zone offset from Greenwich/UTC, such as +02:00.")
  }

  private def attempt[A, E](value: Option[String], parse: String => A, typeName: String): IO[String, A] =
    (ZIO.fromOption(value) orElseFail s"$typeName options do not have a default value.").flatMap { value =>
      ZIO.attempt(parse(value)) orElseFail s"$value is not a $typeName."
    }
}
