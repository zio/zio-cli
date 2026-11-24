package zio.cli

/**
 * The error that parsing a command line fails with: a [[ValidationErrorType]] that says what went wrong, and a
 * `HelpDoc` message for the user. `Command#parse`, `Options.validate` and `Args#validate` fail with it, and
 * `Options#mapOrFail` lets your own checks fail with it.
 *
 * {{{
 * import zio._
 *
 * val command = Command("greet", Options.integer("count"))
 * val parsed: IO[ValidationError, CommandDirective[BigInt]] =
 *   command.parse(List("greet", "--count", "ten"), CliConfig.default)
 * // fails with ValidationErrorType.InvalidValue and the message "ten is not a integer."
 * }}}
 *
 * It extends `Throwable`, but `getMessage` returns `null`: the message is in `error`.
 *
 * @param validationErrorType
 *   The kind of error.
 * @param error
 *   The message for the user.
 * @see
 *   [[https://zio.dev/zio-cli/commands/ Commands]]
 */
final case class ValidationError(validationErrorType: ValidationErrorType, error: HelpDoc) extends Throwable {

  /**
   * Returns `true` when the type is `ValidationErrorType.MissingValue`, as for a required option that is absent.
   * `Options#withDefault` and `Options#optional` use it to return the default instead. A missing positional argument
   * has the type `InvalidArgument`, so this returns `false` for it.
   */
  def isOptionMissing: Boolean = validationErrorType == ValidationErrorType.MissingValue
}

/**
 * The kind of a [[ValidationError]]. Some kinds, such as `MissingFlag` and `UnclusteredFlag`, are used inside the
 * parser to decide what to try next.
 */
sealed trait ValidationErrorType

/** The kinds of [[ValidationError]]. */
object ValidationErrorType {
  // needed because KeyValueMap depends on a Single[String] option
  /**
   * An option was given more than one value in the map that `Options.Single#validate` reads. `Options.keyValueMap`,
   * which collects several `key=value` words for one option, reads the values from it.
   *
   * @param keyValues
   *   The values the option received.
   */
  case class KeyValuesDetected(keyValues: List[String]) extends ValidationErrorType

  /**
   * A cluster of short flags, such as `-al`, was found. The options parser fails with it and then retries with the
   * single flags `-a` and `-l`.
   *
   * @param unclustered
   *   The single flags, such as `List("-a", "-l")`.
   * @param tail
   *   The arguments after the cluster.
   */
  case class UnclusteredFlag(unclustered: List[String], tail: List[String]) extends ValidationErrorType

  /**
   * A value was found but is not valid, such as `ten` for an integer option (`ten is not a integer.`).
   */
  case object InvalidValue extends ValidationErrorType

  /**
   * A required option is absent (`Expected to find --count option.`), or an option that takes a value is the last word
   * (`Expected some value.`). `ValidationError#isOptionMissing` returns `true` only for this type.
   */
  case object MissingValue extends ValidationErrorType

  /**
   * An option did not match the word at the current position. The options parser uses it to move on to the next option.
   */
  case object MissingFlag extends ValidationErrorType

  /**
   * A word looks like a misspelled option name, as in `The flag "--nmae" is not recognized. Did you mean --name?`. It
   * is produced when the option name is longer than `CliConfig#autoCorrectLimit` plus one and the edit distance between
   * the word and the option's full name (such as `--name`) is at most `autoCorrectLimit`.
   */
  case object CorrectedFlag extends ValidationErrorType

  /**
   * The arguments don't start with the command's name (`Missing command name: greet`), or name a subcommand that
   * doesn't exist (`Invalid subcommand for git. Use one of 'add', 'rm'`).
   */
  case object CommandMismatch extends ValidationErrorType

  /**
   * ZIO CLI's own parsers don't produce this type: a parent command given without a subcommand shows its help instead.
   */
  case object MissingSubCommand extends ValidationErrorType

  /**
   * A positional argument is missing or invalid, an `Args#mapOrFail` function returned `Left`, or a `keyValueMap` entry
   * has no `=` (`Expected a key/value pair but got 'a'.`).
   */
  case object InvalidArgument extends ValidationErrorType

  /**
   * The arguments start with the command's name but are not a built-in option such as `--help`. `Command#parse` fails
   * with it internally and then parses the command's own options and arguments.
   */
  case object NoBuiltInMatch extends ValidationErrorType
}
