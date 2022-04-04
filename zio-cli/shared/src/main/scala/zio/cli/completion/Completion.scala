package zio.cli.completion

import zio._
import zio.cli._

object Completion {
  import RegularLanguage._

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
    val (beforeCursor, _) = words.splitAt(index)

    /*
     * Uncluster any flags that are clustered. For example:
     *   "-abc"
     * becomes
     *   "-a" "-b" "-c"
     */
    val unclustered = Command.unCluster(beforeCursor)

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
    val derivative: UIO[RegularLanguage] = ZIO.foldLeft(unclustered)(language)((lang, word) =>
      lang
        .derive(word)
        .provideService(cliConfig)
    )

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
   * Returns a `RegularLanguage` whose accepted language is equivalent to the
   * language accepted by the provided `Command`.
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
   * Returns a `RegularLanguage` whose accepted language is equivalent to the
   * language accepted by the provided `Options`.
   */
  def toRegularLanguage(options: Options[Any]): RegularLanguage =
    options match {
      case Options.Empty =>
        Epsilon
      case Options.WithDefault(options, _, _) =>
        toRegularLanguage(options).?
      case single @ Options.Single(_, _, _: PrimType.Bool, _) =>
        single.names.foldLeft[RegularLanguage](Empty)((lang, name) => lang | StringToken(name))
      case single @ Options.Single(_, _, primType, _) =>
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
    }

  /**
   * Returns a `RegularLanguage` whose accepted language is equivalent to the
   * language accepted by the provided `Args`.
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
