package zio.cli.completion

import zio._
import zio.cli._

/**
 * Computes the tab completions for a command line. `CliApp#run` calls [[Completion.complete]] for the built-in
 * `--shell-completion-index` option, which the script of [[CompletionScript]] passes when the user presses tab.
 */
object Completion {
  import RegularLanguage._

  /**
   * Returns the completions, sorted, for the word at position `index` of `words`. `words` is the whole command line,
   * starting with the name of the executable, which may be any word, so an alias or a renamed executable completes too.
   * The words before `index` select what can come next, and the word at `index` (or `""` when `index` is past the end
   * of `words`) is the prefix that each completion starts with; words after `index` are not read.
   *
   * A completion that finishes a word ends with a space (`"add "`); a directory name ends with `/` instead. The prefix
   * is compared with case, even when `cliConfig.caseSensitive` is `false`; `cliConfig` decides how the words before
   * `index` are matched. A word before `index` that the command does not accept gives an empty list, and so does
   * `index = 0`, the position of the executable's name. `compgen` lists the file and directory names for path options
   * and arguments.
   *
   * {{{
   * import zio._
   * import zio.cli._
   *
   * val git: Command[Any] =
   *   Command("git", Options.boolean("verbose")).subcommands(
   *     Command("add", Args.text("pathspec")),
   *     Command("remove", Args.text("pathspec"))
   *   )
   *
   * val all: UIO[List[String]]    = Completion.complete(List("git"), 1, git, CliConfig.default)
   * // List("--verbose ", "add ", "remove ")
   * val prefix: UIO[List[String]] = Completion.complete(List("git", "r"), 1, git, CliConfig.default)
   * // List("remove ")
   * val none: UIO[List[String]]   = Completion.complete(Nil, 0, git, CliConfig.default)
   * // List()
   * }}}
   */
  def complete(
    words: List[String],
    index: Int,
    command: Command[Any],
    cliConfig: CliConfig,
    compgen: Compgen = Compgen.live
  ): UIO[List[String]] = {
    /*
     * Split the input words into two chunks:
     * 1. The chunk that is strictly before the cursor, and
     * 2. The chunk that is at or after the cursor.
     */
    val (splitted, _) = words.splitAt(index)

    /*
     * Calculate the `RegularLanguage` corresponding to the input command.
     *
     * Here, we allow the top-most `Command.Single.name` field to vary by setting
     * `allowAlias = true`. This is because the first argument will be the name
     * of the executable that is provided when the shell makes a tab completion
     * request. Without doing so, tab completion would fail if the executable
     * were renamed or invoked via an alias.
     */
    val language = toRegularLanguage(command, allowAlias = true)

    /*
     * Repeatedly differentiate the language w.r.t. each of the tokens that
     * occur before the cursor.
     */
    val derivative: UIO[RegularLanguage] =
      ZIO.foldLeft(splitted)(language)((lang, word) => lang.derive(word, cliConfig))

    val wordToComplete = if (index < words.size) words(index) else ""

    /*
     * Finally, obtain the list of completions for the wordToComplete by
     *   1) Getting the list of all of the first tokens in the derivative, and
     *   2) Retaining only those tokens that start with wordToComplete.
     */
    derivative.flatMap(lang =>
      lang
        .firstTokens(wordToComplete, compgen)
        .map(
          _.toList.sorted
            .filter(_.nonEmpty)
        )
    )
  }

  /**
   * Returns the [[RegularLanguage]] of the command lines that `command` accepts, as tab completion reads them: the
   * command name, then its options in any order, then its arguments; for subcommands, the parent's words followed by
   * those of one child. With `allowAlias = true`, the name of the top command matches any word (an
   * [[RegularLanguage.AnyStringToken]]), so completion works when the executable is renamed or called through an alias;
   * the names of subcommands always have to match.
   */
  def toRegularLanguage(command: Command[Any], allowAlias: Boolean): RegularLanguage = command match {
    case Command.Single(name, _, options, args) =>
      val commandNameToken = if (allowAlias) AnyStringToken else StringToken(name)
      commandNameToken ~ toRegularLanguage(options) ~ toRegularLanguage(args)
    case Command.Map(command, _) =>
      toRegularLanguage(command, allowAlias)
    case Command.OrElse(left, right) =>
      toRegularLanguage(left, allowAlias) | toRegularLanguage(right, allowAlias)
    case Command.Subcommands(parent, child) =>
      toRegularLanguage(parent, allowAlias) ~ toRegularLanguage(child, false)
  }

  /**
   * Returns the [[RegularLanguage]] of the words that `options` accepts: each option's name (or one of its aliases)
   * followed by a value, or the name alone for a boolean flag. Options combined with `++` become a
   * [[RegularLanguage.Permutation]], so they can come in any order, each one once, and an option with a default value
   * (`withDefault`, `optional`, or a boolean flag) may be left out.
   */
  def toRegularLanguage(options: Options[Any]): RegularLanguage =
    options match {
      case Options.Empty =>
        Epsilon
      case Options.WithDefault(options, _) =>
        toRegularLanguage(options).?
      case single @ Options.Single(_, _, _: PrimType.Bool, _, _) =>
        single.names.foldLeft[RegularLanguage](Empty)((lang, name) => lang | StringToken(name))
      case single @ Options.Single(_, _, primType, _, _) =>
        val names = single.names.foldLeft[RegularLanguage](Empty)((lang, name) => lang | StringToken(name))
        names ~ PrimTypeToken(primType)
      case Options.OrElse(left, right) =>
        toRegularLanguage(left) | toRegularLanguage(right)
      case Options.Both(left, right) =>
        val leftLanguage  = toRegularLanguage(left)
        val rightLanguage = toRegularLanguage(right)

        // Deforestation
        (leftLanguage, rightLanguage) match {
          case (Permutation(xs @ _*), Permutation(ys @ _*)) => Permutation((xs ++ ys): _*)
          case (Permutation(xs @ _*), _)                    => Permutation((xs :+ rightLanguage): _*)
          case (_, Permutation(ys @ _*))                    => Permutation((ys :+ leftLanguage): _*)
          case _                                            => Permutation(leftLanguage, rightLanguage)
        }
      case Options.Map(value, _) =>
        toRegularLanguage(value)
      case Options.KeyValueMap(argumentOption) =>
        val optionGrammar = toRegularLanguage(argumentOption)
        Permutation(optionGrammar)
      case o: Options.OAuth2Options =>
        toRegularLanguage(o.auxiliaryOptions)
    }

  /**
   * Returns the [[RegularLanguage]] of the words that `args` accepts: one word per single argument, in order, and a
   * repeated argument (`*`, `+`, `atLeast`, `atMost`, `between`) as a [[RegularLanguage.Rep]] with the same bounds.
   */
  def toRegularLanguage(args: Args[Any]): RegularLanguage = args match {
    case Args.Empty =>
      Epsilon
    case Args.Single(_, primType, _) =>
      PrimTypeToken(primType)
    case Args.Both(head, tail) =>
      toRegularLanguage(head) ~ toRegularLanguage(tail)
    case Args.Variadic(value, minOpt, maxOpt) =>
      toRegularLanguage(value).rep(minOpt, maxOpt)
    case Args.Map(value, _) =>
      toRegularLanguage(value)
  }
}
