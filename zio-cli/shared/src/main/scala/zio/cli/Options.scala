package zio.cli

import zio.cli.HelpDoc.Span._
import zio.cli.HelpDoc.p
import zio.{Console, IO, UIO, ZIO, Zippable}
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
 * An `Options[A]` describes the named options of a command, such as `--verbose` or `--port 8080`, and produces a value
 * of type `A` once parsed.
 *
 * Create one with a constructor such as `Options.text("name")` or `Options.boolean("verbose")`, combine several with
 * `++`, offer alternatives with `|`, make one optional with `optional` or `withDefault`, and add names and help with
 * `alias` and `??`. A one-letter name is typed with one dash (`-v`) and a longer one with two (`--verbose`). The value
 * follows as the next word, or after `=` for a long name (`--name=Ann`).
 *
 * {{{
 * val verbose: Options[Boolean]         = Options.boolean("verbose").alias("v")
 * val port: Options[BigInt]             = Options.integer("port").withDefault(BigInt(8080))
 * val both: Options[(Boolean, BigInt)]  = verbose ++ port
 * }}}
 *
 * @see
 *   [[https://zio.dev/zio-cli/options/ Options]]
 */
sealed trait Options[+A] extends Parameter { self =>

  import Options.Single

  /**
   * Combines this option with `that`. Both are read from the command line, in either order, and their values are zipped
   * into a flat tuple (`a ++ b ++ c` gives a triple); a `Unit` value is dropped. When both are missing, the error lists
   * both.
   */
  final def ++[A1 >: A, B](that: Options[B])(implicit zippable: Zippable[A, B]): Options[zippable.Out] =
    Options.Both(self, that).map { case (a, b) => zippable.zip(a, b) }

  /**
   * Same as `orElse`.
   */
  final def |[A1 >: A](that: Options[A1]): Options[A1] = orElse(that)

  /**
   * Accepts either this option or `that`, and returns the value of the one given. When both are given, it fails with
   * `Options collision detected. You can only specify either --file or --url.` (with the two options' names); when
   * neither is given, the error lists both.
   */
  final def orElse[A1 >: A](that: Options[A1]): Options[A1] =
    orElseEither(that).map(_.merge)

  /**
   * Like `orElse`, but returns `Left` with the value of this option or `Right` with the value of `that`.
   */
  final def orElseEither[B](that: Options[B]): Options[Either[A, B]] =
    Options.OrElse(self, that)

  /**
   * Returns a copy with `that` added as a paragraph to the description shown in help. On a combination, the paragraph
   * is added to every option inside.
   */
  def ??(that: String): Options[A] =
    modifySingle(new SingleModifier {
      override def apply[A1](single: Single[A1]): Single[A1] = single.copy(description = single.description + p(that))
    })

  /**
   * Adds more names for this option. A one-letter name is typed with one dash (`alias("v")` accepts `-v`) and a longer
   * one with two. On a combination such as `a ++ b`, every option inside gets the names, so call it on each option
   * before combining.
   */
  def alias(name: String, names: String*): Options[A] =
    modifySingle(new SingleModifier {
      override def apply[A1](single: Single[A1]): Single[A1] = single.copy(aliases = single.aliases ++ (name +: names))
    })

  /**
   * Applies `f` to the two values of a pair-valued option, for example to build a case class (`.as(Bounds.apply _)`).
   */
  final def as[B, C, Z](f: (B, C) => Z)(implicit ev: A <:< (B, C)): Options[Z] =
    map(ev).map { case (b, c) => f(b, c) }

  /**
   * Applies `f` to the three values of a tuple-valued option, for example to build a case class.
   */
  final def as[B, C, D, Z](f: (B, C, D) => Z)(implicit ev: A <:< (B, C, D)): Options[Z] =
    map(ev).map { case (b, c, d) => f(b, c, d) }

  /**
   * Applies `f` to the four values of a tuple-valued option, for example to build a case class.
   */
  final def as[B, C, D, E, Z](f: (B, C, D, E) => Z)(implicit ev: A <:< (B, C, D, E)): Options[Z] =
    map(ev).map { case (b, c, d, e) => f(b, c, d, e) }

  /**
   * Applies `f0` to the five values of a tuple-valued option, for example to build a case class.
   */
  final def as[B, C, D, E, F, Z](f0: (B, C, D, E, F) => Z)(implicit ev: A <:< (B, C, D, E, F)): Options[Z] =
    map(ev).map { case (b, c, d, e, f) => f0(b, c, d, e, f) }

  /**
   * Applies `f0` to the six values of a tuple-valued option, for example to build a case class.
   */
  final def as[B, C, D, E, F, G, Z](
    f0: (B, C, D, E, F, G) => Z
  )(implicit ev: A <:< (B, C, D, E, F, G)): Options[Z] =
    map(ev).map { case (b, c, d, e, f, g) => f0(b, c, d, e, f, g) }

  /**
   * Turns the `Either` value of an option built with `orElseEither` into one value: `f1` handles the `Left` side and
   * `f2` the `Right` side.
   */
  final def fold[B, C, Z](
    f1: B => Z,
    f2: C => Z
  )(implicit ev: A <:< Either[B, C]): Options[Z] =
    map(ev).map {
      case Left(b)  => f1(b)
      case Right(c) => f2(c)
    }

  /**
   * Turns the nested `Either` value of three options chained with `orElseEither` into one value, with one function per
   * option, from left to right.
   */
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

  /**
   * Turns the nested `Either` value of four options chained with `orElseEither` into one value, with one function per
   * option, from left to right.
   */
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

  /**
   * Turns the nested `Either` value of five options chained with `orElseEither` into one value, with one function per
   * option, from left to right.
   */
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

  /**
   * Turns the nested `Either` value of six options chained with `orElseEither` into one value, with one function per
   * option, from left to right.
   */
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

  /**
   * Transforms the parsed value with the partial function `f`. A value that `f` doesn't cover makes parsing fail with
   * `message` as the error, as a [[ValidationError]] of type `InvalidValue`.
   */
  final def collect[B](message: String)(f: PartialFunction[A, B]): Options[B] =
    Options.Map(
      self,
      (a: A) =>
        f.lift(a)
          .fold[Either[ValidationError, B]](Left(ValidationError(ValidationErrorType.InvalidValue, p(error(message)))))(
            Right(_)
          )
    )

  /**
   * Returns the options inside this one that read words from the command line (single options and key-value options),
   * from left to right.
   */
  def flatten: List[Options[_] with Input]

  /**
   * Returns the help text of this option, as shown in the options section of a command's help.
   */
  def helpDoc: HelpDoc

  /**
   * Returns `true` when this option is a boolean flag, directly or under `map`, `withDefault` or `optional`. A flag
   * with negation names (an `orElse` of two flags) and a combination return `false`.
   */
  final def isBool: Boolean =
    asInstanceOf[Options[_]] match {
      case Options.Empty                   => false
      case Options.WithDefault(options, _) => options.isBool
      case Single(_, _, primType, _, _)    => primType.isBool
      case Options.Map(value, _)           => value.isBool
      case _                               => false
    }

  /**
   * Transforms the parsed value with `f`.
   */
  final def map[B](f: A => B): Options[B] = Options.Map(self, (a: A) => Right(f(a)))

  /**
   * Transforms the parsed value with `f`. A `Left` makes parsing fail with that error.
   */
  final def mapOrFail[B](f: A => Either[ValidationError, B]): Options[B] =
    Options.Map(self, (a: A) => f(a))

  /**
   * Makes this option optional: returns `Some(value)` when it is given and `None` when it is absent. Help adds
   * `This setting is optional.`, and the usage line shows the option in square brackets. A value that is given but
   * invalid still fails.
   */
  final def optional: Options[Option[A]] = map(Some(_)).withDefault(None)

  /**
   * Returns the usage synopsis of this option, such as `--name <text>`, used in a command's usage line.
   */
  def synopsis: UsageSynopsis

  /**
   * Returns the main name of each option inside, with its dashes, joined with `, ` (`Some("--name, --age")`), or `None`
   * for `Options.none`.
   */
  def uid: Option[String]

  /**
   * Builds the value of this option from `args`, a map from the option names typed on the command line (with their
   * dashes) to the words that followed them. `Options.validate` builds that map from the command line and then calls
   * this method.
   */
  def validate(args: Predef.Map[String, List[String]], conf: CliConfig): IO[ValidationError, A]

  /**
   * Makes this option optional with a default: returns `value` when the option is absent. Help adds
   * `This setting is optional. Default: '8080'.` (with the default's `toString`), and the usage line shows the option
   * in square brackets. A value that is given but invalid still fails. On a combination such as `a ++ b`, the help text
   * is left unchanged.
   */
  def withDefault[A1 >: A](value: A1): Options[A1] =
    Options.WithDefault(self, value)

  /**
   * Sets the placeholder shown for the value of this option in help and in the usage line: `withPseudoName("who")`
   * shows `--name <who>` instead of the default `--name <text>`, which uses the type name. An enumeration shows its
   * accepted words instead of a placeholder, so the pseudo name doesn't appear for it. On a combination, every option
   * inside gets the placeholder.
   */
  def withPseudoName(name: String): Options[A] =
    modifySingle(new SingleModifier {
      override def apply[A2](single: Single[A2]): Single[A2] =
        single.copy(pseudoName = Some(name))
    })

  private[cli] def modifySingle(f: SingleModifier): Options[A]

  /**
   * The word `option`, which the wizard mode uses to name this kind of parameter.
   */
  lazy val tag = "option"
}

/**
 * A change applied to a single option, whatever its value type. `??`, `alias` and `withPseudoName` use one to change
 * every single option inside a combination.
 */
trait SingleModifier {

  /**
   * Returns a changed copy of `single`.
   */
  def apply[A](single: Options.Single[A]): Options.Single[A]
}

/**
 * Constructors for [[Options]]. Each value type has a method that takes the option's name. A value that doesn't parse
 * fails with `<value> is not a <type>.`, such as `x is not a integer.`; an option that is required and absent fails
 * with `Expected to find --name option.`.
 */
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

  // Adds the entries of `map2` to `map1`; for a key in both, appends the values of `map2` to those of `map1`.
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
   * Returns the error that stopped option matching (if any), the words of `input` that were not read as options, and a
   * `Map` linking each option name to its values.
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

  private def allOptionNames(options: Options[_]): Set[String] =
    options.flatten.flatMap {
      case s: Single[_]    => s.names
      case kv: KeyValueMap => kv.argumentOption.names
      case _               => Nil
    }.toSet

  private def conflictingOptionNames(flag: String, options: Options[_], conf: CliConfig): Set[String] = {
    val normFlag                                       = conf.normalizeCase(flag)
    def containsFlag(names: Iterable[String]): Boolean =
      names.exists(n => conf.normalizeCase(n) == normFlag)

    options match {
      case Empty        => Set(flag)
      case s: Single[_] =>
        if (containsFlag(s.names)) s.names.toSet + flag else Set.empty
      case kv: KeyValueMap =>
        if (containsFlag(kv.argumentOption.names)) kv.argumentOption.names.toSet + flag else Set.empty
      case WithDefault(inner, _) =>
        conflictingOptionNames(flag, inner, conf)
      case Map(inner, _) =>
        conflictingOptionNames(flag, inner, conf)
      case Both(left, right) =>
        conflictingOptionNames(flag, left, conf) ++ conflictingOptionNames(flag, right, conf)
      case OrElse(left, right) =>
        val leftNames  = allOptionNames(left)
        val rightNames = allOptionNames(right)
        val inLeft     = containsFlag(leftNames)
        val inRight    = containsFlag(rightNames)
        (if (inLeft) conflictingOptionNames(flag, left, conf) ++ rightNames else Set.empty) ++
          (if (inRight) conflictingOptionNames(flag, right, conf) ++ leftNames else Set.empty)
      case OAuth2Options(_, _, aux) =>
        conflictingOptionNames(flag, aux, conf)
    }
  }

  private def matchFileOptions(
    input: List[String],
    options: List[Options[_] with Input],
    conf: CliConfig
  ): IO[ValidationError, Predef.Map[String, List[String]]] =
    (input, options) match {
      case (Nil, _) | (_, Nil) => ZIO.succeed(Predef.Map.empty)
      case (_ :: tail, opts)   =>
        findOptions(input, opts, conf).foldZIO(
          e =>
            e.validationErrorType match {
              case ValidationErrorType.MissingFlag | ValidationErrorType.CorrectedFlag |
                  ValidationErrorType.UnclusteredFlag(_, _) =>
                matchFileOptions(tail, opts, conf)
              case _ =>
                ZIO.fail(e)
            },
          { case (otherArgs, otherOptions, map1) =>
            if (map1.isEmpty) matchFileOptions(tail, opts, conf)
            else matchFileOptions(otherArgs, otherOptions, conf).map(map2 => merge(map1, map2.toList))
          }
        )
    }

  private def mergeFileOptions(
    queue: List[FileOptions.OptionsFromFile],
    acc: Predef.Map[String, (String, List[String])],
    rootOptions: Options[_],
    flattenedOptions: List[Options[_] with Input],
    conf: CliConfig
  ): IO[ValidationError, Predef.Map[String, (String, List[String])]] =
    queue match {
      case FileOptions.OptionsFromFile(path, rawArgs) :: tail =>
        for {
          matchedMap <- matchFileOptions(rawArgs, flattenedOptions, conf).mapError(e =>
                          ValidationError(
                            e.validationErrorType,
                            HelpDoc.p(s"Error in options file '$path':") + e.error
                          )
                        )
          newMap               = matchedMap.map { case (k, v) => (k, (path, v)) }
          overriddenNormalized =
            newMap.keys.flatMap(k => conflictingOptionNames(k, rootOptions, conf)).map(conf.normalizeCase).toSet
          retainedAcc = acc.filterNot { case (accKey, _) =>
                          overriddenNormalized.contains(conf.normalizeCase(accKey))
                        }
          _ <- ZIO.foreachDiscard(acc.toList) { case (accKey, (accPath, _)) =>
                 ZIO.when(overriddenNormalized.contains(conf.normalizeCase(accKey))) {
                   ZIO.logDebug(s"Option '$accKey' from '$accPath' overridden by '$path'")
                 }
               }
          mergedMap = retainedAcc ++ newMap
          res      <- mergeFileOptions(tail, mergedMap, rootOptions, flattenedOptions, conf)
        } yield res
      case Nil =>
        ZIO.succeed(acc)
    }

  private def mergeFileAndInputOptions(
    inputOptions: Predef.Map[String, List[String]],
    fileOptions: Predef.Map[String, (String, List[String])],
    rootOptions: Options[_],
    conf: CliConfig
  ): UIO[(Predef.Map[String, List[String]], List[String])] = {
    val cliOverriddenNormalized =
      inputOptions.keys.flatMap(k => conflictingOptionNames(k, rootOptions, conf)).map(conf.normalizeCase).toSet

    for {
      fromFilesRetained <-
        ZIO.foreach(fileOptions.toList.sortBy(_._1)) { case (k, (filePath, fileVals)) =>
          if (cliOverriddenNormalized.contains(conf.normalizeCase(k))) {
            ZIO.logDebug(s"Option '$k' from '$filePath' overridden by command-line arguments").as(None)
          } else {
            val diagnostic = s"Using option '$k' from '$filePath'"
            ZIO.succeed(Some((k -> fileVals, diagnostic)))
          }
        }
      retained = fromFilesRetained.flatten
    } yield (retained.map(_._1).toMap ++ inputOptions, retained.map(_._2))
  }

  private[cli] def emitDiagnostics(diagnostics: List[String]): UIO[Unit] =
    ZIO.foreachDiscard(diagnostics)(d => Console.printLineError(d).ignore *> ZIO.logDebug(d))

  private[cli] def validateWithDiagnostics[A](
    options: Options[A],
    args: List[String],
    conf: CliConfig,
    fromFiles: List[FileOptions.OptionsFromFile]
  ): IO[ValidationError, (Option[ValidationError], List[String], A, List[String])] = {
    val flattened = options.flatten
    for {
      matched                             <- matchOptions(args, flattened, conf)
      (error, commandArgs, matchedOptions) = matched
      mapFromFiles                        <- mergeFileOptions(fromFiles.reverse, Predef.Map.empty, options, flattened, conf)
      mergedWithDiag                      <- mergeFileAndInputOptions(matchedOptions, mapFromFiles, options, conf)
      (mergedOptions, diagnostics)         = mergedWithDiag
      a                                   <- options.validate(mergedOptions, conf).mapError(error.getOrElse(_))
    } yield (error, commandArgs, a, diagnostics)
  }

  /**
   * Parses the options at the front of `args` and builds the value of `options`. Returns the words that come after the
   * options (the command's arguments), the value, and, when matching stopped at a word it couldn't read as an option,
   * the error it met there. That error is returned rather than raised because it may only mean that the options ended;
   * `Command` reports it when its arguments then fail to parse. Fails when the value can't be built, with the error met
   * during matching if there was one, and otherwise with the error of `options`.
   */
  def validate[A](
    options: Options[A],
    args: List[String],
    conf: CliConfig
  ): IO[ValidationError, (Option[ValidationError], List[String], A)] =
    validate(options, args, conf, Nil)

  /**
   * Like `validate(options, args, conf)`, while also merging fallback options from `fromFiles` (ordered from highest
   * file priority to lowest file priority) and printing diagnostics for options supplied from files.
   */
  def validate[A](
    options: Options[A],
    args: List[String],
    conf: CliConfig,
    fromFiles: List[FileOptions.OptionsFromFile]
  ): IO[ValidationError, (Option[ValidationError], List[String], A)] =
    for {
      res                       <- validateWithDiagnostics(options, args, conf, fromFiles)
      (error, commandArgs, a, d) = res
      _                         <- emitDiagnostics(d)
    } yield (error, commandArgs, a)

  /**
   * The `Options` that reads nothing and produces `()`; `Options.none` returns it.
   */
  case object Empty extends Options[Unit] with Pipeline {

    /**
     * Always `UsageSynopsis.None`.
     */
    lazy val synopsis: UsageSynopsis = UsageSynopsis.None

    override def flatten: List[Options[_] with Input] = Nil

    override def validate(args: Predef.Map[String, List[String]], conf: CliConfig): IO[ValidationError, Unit] = ZIO.unit

    override def modifySingle(f: SingleModifier): Options[Unit] = Empty

    override lazy val helpDoc: HelpDoc = HelpDoc.Empty

    override lazy val uid: Option[String] = None

    override def pipeline = ("", List())
  }

  /**
   * An option that produces `default` when `options` is absent. `withDefault` and `optional` build it.
   */
  final case class WithDefault[A](options: Options[A], default: A) extends Options[A] with Input {

    override lazy val shortDesc: String = options.shortDesc

    /**
     * The synopsis of `options`, in square brackets (`[--port <integer>]`).
     */
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

  /**
   * One named option: its `name` and `aliases` (without dashes), the `primType` that parses its value, its help
   * `description`, and an optional `pseudoName` for the placeholder of its value.
   */
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

    /**
     * The names, one-letter names first, followed by the placeholder of the value (or the accepted words of an
     * enumeration); a boolean flag has no placeholder. With more than one name, the whole is put in parentheses:
     * `(-n, --name <text>)`.
     */
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

    /**
     * The main name with its dashes, such as `Some("--name")` or `Some("-n")`.
     */
    lazy val uid: Option[String] = Some(fullName)

    private def makeFullName(s: String): (Boolean, String) = if (s.length == 1) (true, "-" + s) else (false, "--" + s)

    /**
     * All names with their dashes, one-letter names first, such as `List("-n", "--name")`.
     */
    lazy val names: List[String] = (name :: aliases.toList).map(makeFullName).sortBy(!_._1).map(_._2)

    private lazy val fullName: String = makeFullName(name)._2

    override lazy val helpDoc: HelpDoc =
      HelpDoc.DescriptionList(List(synopsis.helpDoc.getSpan -> (p(primType.helpDoc) + description)))

    override def isValid(input: String, conf: CliConfig): IO[ValidationError, List[String]] = for {
      _ <- parse(List(names.head, input), conf)
    } yield List(names.head, input)

    private def placeholder: String = "<" + pseudoName.getOrElse(primType.typeName) + ">"
  }

  /**
   * Two alternative options; `orElse`, `orElseEither` and `|` build it.
   */
  final case class OrElse[A, B](left: Options[A], right: Options[B]) extends Options[Either[A, B]] with Alternatives {

    override def modifySingle(f: SingleModifier): Options[Either[A, B]] =
      OrElse(left.modifySingle(f), right.modifySingle(f))

    /**
     * The two synopses separated by `|`, such as `--file <text>|--url <text>`.
     */
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

  /**
   * Two options that are both read; `++` builds it.
   */
  final case class Both[A, B](left: Options[A], right: Options[B]) extends Options[(A, B)] with Pipeline {
    override def modifySingle(f: SingleModifier): Options[(A, B)] =
      Both(left.modifySingle(f), right.modifySingle(f))

    /**
     * The two synopses separated by a space.
     */
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

  /**
   * An option whose value is transformed by `f`; a `Left` from `f` makes parsing fail. `map`, `mapOrFail`, `collect`
   * and `optional` build it.
   */
  final case class Map[A, B](value: Options[A], f: A => Either[ValidationError, B])
      extends Options[B]
      with Pipeline
      with Wrap {

    override lazy val shortDesc = value.shortDesc

    override def modifySingle(f0: SingleModifier): Options[B] = Map(value.modifySingle(f0), f)

    /**
     * The synopsis of `value`.
     */
    lazy val synopsis: UsageSynopsis = value.synopsis

    override def flatten: List[Options[_] with Input] = value.flatten

    /**
     * Builds the value of `value`, then applies `f`. A `Left` fails with the returned error.
     */
    def validate(args: Predef.Map[String, List[String]], conf: CliConfig): IO[ValidationError, B] =
      value.validate(args, conf).flatMap(a => f(a).fold(e => ZIO.fail(e), s => ZIO.succeed(s)))

    override lazy val uid: Option[String] = value.uid

    override lazy val helpDoc: HelpDoc = value.helpDoc

    override def pipeline = ("", List(value))

    override val wrapped = value
  }

  /**
   * An option that collects `key=value` pairs into a `Map`, read with the names of `argumentOption`.
   * `Options.keyValueMap` builds it.
   */
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

  /**
   * An option that produces an OAuth2 access token from `provider` for the given `scope`. `auxiliaryOptions` are the
   * extra options it reads, such as the file that stores the token. `Options.oauth2` builds it.
   */
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
   * Creates a boolean flag with the given name: present, it produces `ifPresent`; absent, it produces `!ifPresent`. A
   * `true` or `false` word right after the flag sets the value explicitly (`--verbose false`).
   */
  def boolean(name: String, ifPresent: Boolean = true): Options[Boolean] = makeBoolean(name, ifPresent, Nil)

  /**
   * Creates a boolean flag with the given name and one or more negation names: the flag produces `ifPresent`, a
   * negation name produces `!ifPresent`, and when neither is given the value is `!ifPresent`. Giving both fails with
   * `Options collision detected. You can only specify either --color or --no-color.`.
   *
   * {{{
   * val color: Options[Boolean] = Options.boolean("color", true, "no-color")
   * }}}
   */
  def boolean(name: String, ifPresent: Boolean, negationName: String, negationNames: String*): Options[Boolean] =
    makeBoolean(name, ifPresent, negationName :: negationNames.toList)

  /**
   * Creates a boolean flag with the given name, an `alias` that is accepted as well (`-c` for `--color`), and one or
   * more negation names. The flag and its alias produce `ifPresent`, a negation name produces `!ifPresent`, and when
   * none is given the value is `!ifPresent`.
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
   * Creates an option whose value is one of `cases`. Each case pairs the word the user types with the value it
   * produces; the word must match exactly. Help and the usage line show the accepted words, and any other word fails
   * with `Expected one of the following cases: ...`.
   *
   * {{{
   * val level: Options[Int] = Options.enumeration("level")("low" -> 1, "high" -> 2)
   * }}}
   */
  def enumeration[A](name: String)(cases: (String, A)*): Options[A] =
    Single(name, Vector.empty, PrimType.Enumeration(cases: _*))

  /**
   * Creates an option that accepts any text.
   */
  def text(name: String): Options[String] =
    Single(name, Vector.empty, PrimType.Text)

  /**
   * Creates an option that parses a `BigDecimal`, such as `1.5`.
   */
  def decimal(name: String): Options[BigDecimal] =
    Single(name, Vector.empty, PrimType.Decimal)

  /**
   * Creates an option that parses a `java.time.Duration`, a time-based amount of time in the ISO-8601 form, such as
   * `PT1H` or `P1DT2H3M`.
   */
  def duration(name: String): Options[JDuration] =
    Single(name, Vector.empty, PrimType.Duration)

  /**
   * Creates an option that parses a `BigInt`.
   */
  def integer(name: String): Options[BigInt] =
    Single(name, Vector.empty, PrimType.Integer)

  /**
   * Creates an option that parses a `java.time.Instant` in UTC, such as `2007-12-03T10:15:30.00Z`.
   */
  def instant(name: String): Options[JInstant] =
    Single(name, Vector.empty, PrimType.Instant)

  /**
   * Creates an option that parses a `java.time.LocalDate` in the ISO-8601 form, such as `2007-12-03`.
   */
  def localDate(name: String): Options[JLocalDate] =
    Single(name, Vector.empty, PrimType.LocalDate)

  /**
   * Creates an option that parses a `java.time.LocalDateTime`, a date-time without a time zone in the ISO-8601 form,
   * such as `2007-12-03T10:15:30`.
   */
  def localDateTime(name: String): Options[JLocalDateTime] =
    Single(name, Vector.empty, PrimType.LocalDateTime)

  /**
   * Creates an option that parses a `java.time.LocalTime`, a time without a time zone in the ISO-8601 form, such as
   * `10:15:30`.
   */
  def localTime(name: String): Options[JLocalTime] =
    Single(name, Vector.empty, PrimType.LocalTime)

  /**
   * Creates an option that parses a `java.time.MonthDay` in the ISO-8601 form `--MM-DD`, such as `--12-03`. A value
   * without the leading dashes, such as `12-03`, fails.
   */
  def monthDay(name: String): Options[JMonthDay] =
    Single(name, Vector.empty, PrimType.MonthDay)

  /**
   * The `Options` that reads nothing and produces `()`; use it for a command without options.
   */
  val none: Options[Unit] = Empty

  /**
   * Creates an option that parses a `java.time.OffsetDateTime`, a date-time with an offset from UTC in the ISO-8601
   * form, such as `2007-12-03T10:15:30+01:00`.
   */
  def offsetDateTime(name: String): Options[JOffsetDateTime] =
    Single(name, Vector.empty, PrimType.OffsetDateTime)

  /**
   * Creates an option that parses a `java.time.OffsetTime`, a time with an offset from UTC in the ISO-8601 form, such
   * as `10:15:30+01:00`.
   */
  def offsetTime(name: String): Options[JOffsetTime] =
    Single(name, Vector.empty, PrimType.OffsetTime)

  /**
   * Creates an option that parses a `java.time.Period`, a date-based amount of time in the ISO-8601 form, such as
   * `P1Y2M3D`.
   */
  def period(name: String): Options[JPeriod] =
    Single(name, Vector.empty, PrimType.Period)

  /**
   * Creates an option that parses a `java.time.Year`, such as `2007`.
   */
  def year(name: String): Options[JYear] =
    Single(name, Vector.empty, PrimType.Year)

  /**
   * Creates an option that parses a `java.time.YearMonth` in the ISO-8601 form, such as `2007-12`.
   */
  def yearMonth(name: String): Options[JYearMonth] =
    Single(name, Vector.empty, PrimType.YearMonth)

  /**
   * Creates an option that parses a `java.time.ZonedDateTime`, a date-time with a time zone in the ISO-8601 form, such
   * as `2007-12-03T10:15:30+01:00[Europe/Paris]`. The zone ID goes in square brackets right after the offset.
   */
  def zonedDateTime(name: String): Options[JZonedDateTime] =
    Single(name, Vector.empty, PrimType.ZonedDateTime)

  /**
   * Creates an option that parses a `java.time.ZoneId`, such as `Europe/Paris`.
   */
  def zoneId(name: String): Options[JZoneId] =
    Single(name, Vector.empty, PrimType.ZoneId)

  /**
   * Creates an option that parses a `java.time.ZoneOffset`, an offset from UTC, such as `+02:00`.
   */
  def zoneOffset(name: String): Options[JZoneOffset] =
    Single(name, Vector.empty, PrimType.ZoneOffset)

  /**
   * Creates an option that collects `key=value` pairs into a `Map`. The option may be repeated right after its pairs
   * (`--define a=1 --define b=2`) or followed by several pairs (`--define a=1 b=2`); a repeat that comes after another
   * option is not read, and its words are left for the arguments. The pairs end at the first word without `=`. The key
   * is the text before the first `=` and the value the rest (`a=b=c` gives `a -> b=c`). When a key is given twice, the
   * first value is kept. When the first word after the option has no `=` or an empty value (`a=`), the option is not
   * read, so a required key-value option fails with `Expected to find --define option.`.
   *
   * With the default case-insensitive [[CliConfig]], a name with an upper-case letter (`keyValueMap("D")`) is never
   * matched, whether the user types `-D` or `-d`; use a lower-case name, or set `caseSensitive = true`.
   *
   * {{{
   * val defines: Options[Map[String, String]] = Options.keyValueMap("define").alias("d")
   * }}}
   */
  def keyValueMap(name: String): Options[Predef.Map[String, String]] =
    keyValueMap(Options.Single(name, Vector.empty, PrimType.Text))

  /**
   * Creates a key-value option from `argumentOption`, using its names and description. It reads `key=value` pairs as
   * `keyValueMap(name)` does.
   */
  def keyValueMap(argumentOption: Options.Single[String]): Options[Predef.Map[String, String]] =
    Options.KeyValueMap(argumentOption)
}
