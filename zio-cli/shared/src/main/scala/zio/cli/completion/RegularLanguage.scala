package zio.cli.completion

import zio._
import zio.cli.CliConfig
import zio.cli.PrimType

/**
 * A `RegularLanguage` is a set of lists of words (tokens) that tab completion uses to describe the command lines a
 * command accepts. It is an implementation of "Parsing With Derivatives" (Might et al. 2011): `derive` consumes one
 * word, `isNullable` tells whether the empty list is accepted, and `firstTokens` lists the words that can come next. A
 * token is a fixed string ([[RegularLanguage.StringToken]]), any string ([[RegularLanguage.AnyStringToken]]), or any
 * string that a `zio.cli.PrimType` accepts ([[RegularLanguage.PrimTypeToken]]).
 *
 * {{{
 * import zio.cli.CliConfig
 *
 * val lang: RegularLanguage = ("git" ~ "add") | ("git" ~ "remove")
 * lang.contains(List("git", "add"), CliConfig.default) // UIO containing true
 * lang.firstTokens("", Compgen.live)                   // UIO containing Set("git ")
 * }}}
 */
sealed trait RegularLanguage extends Product with Serializable {
  import RegularLanguage._

  /**
   * This is the delta (δ) predicate from "Parsing With Derivatives", indicating whether this language contains the
   * empty list of tokens.
   *
   * @return
   *   true if and only if this language contains the empty list of tokens.
   */
  def isNullable: Boolean

  /**
   * Calculate the Brzozowski derivative of this language with respect to the given string: the language of what can
   * follow `token` in the token lists of this language. This is an effectful function because it can call
   * `PrimType.validate` (e.g., when validating file paths, etc.).
   *
   * @param token
   *   The string to use for calculation of the Brzozowski derivative.
   * @param cliConfig
   *   Decides whether a [[RegularLanguage.StringToken]] matches `token` with or without case; it is also passed to
   *   `PrimType.validate`.
   * @return
   *   Brzozowski derivative wrapped in an UIO instance.
   */
  def derive(token: String, cliConfig: CliConfig): UIO[RegularLanguage]

  /**
   * Returns the concatenation of this language and `other` (a `Cat`): a token list of this language followed by one of
   * `other`.
   */
  def ~(other: RegularLanguage): RegularLanguage = Cat(this, other)

  /** Returns this language followed by the single word `other` (a `Cat` with a `StringToken`). */
  def ~(other: String): RegularLanguage = Cat(this, StringToken(other))

  /** Returns the union of this language and `other` (an `Alt`). */
  def |(other: RegularLanguage): RegularLanguage = Alt(this, other)

  /** Returns the union of this language and the single word `other` (an `Alt` with a `StringToken`). */
  def |(other: String): RegularLanguage = Alt(this, StringToken(other))

  /** Returns zero or more repetitions of this language (a `Rep` with no bounds). */
  def * = Rep(this, None, None): RegularLanguage

  /**
   * Returns this language repeated at least `min` and at most `max` times (a `Rep`); `None` leaves that side unbounded.
   * For example, `rep(Some(1), Some(2))` accepts one or two repetitions.
   */
  def rep(min: Option[Int] = None, max: Option[Int] = None): RegularLanguage = Rep(this, min, max)

  /** Returns this language or the empty list of tokens (`Alt(this, Epsilon)`), which makes it optional. */
  def ? = Alt(this, Epsilon)

  /**
   * Checks to see if the input token list is a member of the language: it derives the language with respect to each
   * token in turn and checks that the result is nullable.
   *
   * @return
   *   true if and only if `tokens` is in the language.
   */
  def contains(tokens: List[String], cliConfig: CliConfig): UIO[Boolean] =
    ZIO
      .foldLeft(tokens)(this)((language, word) => language.derive(word, cliConfig))
      .map(dx => dx.isNullable)

  /**
   * Returns the words that can come first in the token lists of this language and start with `prefix`, for tab
   * completion. A language whose words can't be listed returns no word for them (`AnyStringToken`, or a `PrimTypeToken`
   * of `PrimType.Text`), and the user then sees no completion. `Epsilon` adds the empty string, which
   * `Completion.complete` drops.
   *
   * A word that completes a whole token ends with a space, so that the cursor can move on to the next argument: string
   * tokens (`"add "`), file names (`"bippy.pdf "`), and the values that [[PrimTypeCompletion.firstTokens]] lists. A
   * directory name ends with `/` and no space (`"foo/"`), so that the user can press tab again to complete a longer
   * path.
   *
   * The space is part of the word instead of coming from the `-o nospace` setting of `compgen` and `complete`, because
   * that setting applies to all the words of one tab completion, which does not work when file names and directory
   * names are completed together.
   */
  def firstTokens(prefix: String, compgen: Compgen): UIO[Set[String]]
}

/**
 * The cases of [[RegularLanguage]]: the empty language, the language of the empty list, single tokens, and their
 * combinations.
 */
object RegularLanguage {

  /**
   * The `Empty` language (∅) accepts no list of tokens, not even the empty one.
   */
  case object Empty extends RegularLanguage {

    /** Returns `false`. */
    def isNullable: Boolean = false

    /** Returns `Empty`. */
    def derive(token: String, cliConfig: CliConfig) = ZIO.succeed(Empty)

    /** Returns an empty set. */
    def firstTokens(prefix: String, compgen: Compgen): UIO[Set[String]] =
      ZIO.succeed(Set.empty)

    override def toString = "∅"
  }

  /**
   * The `Epsilon` language (ε) accepts only the empty list of tokens.
   */
  case object Epsilon extends RegularLanguage {

    /** Returns `true`. */
    def isNullable: Boolean = true

    /** Returns `Empty`: no token can follow. */
    def derive(token: String, cliConfig: CliConfig) = ZIO.succeed(Empty)

    /**
     * Returns a set holding only the empty string, whatever `prefix` is. `Completion.complete` drops the empty string.
     */
    def firstTokens(prefix: String, compgen: Compgen): UIO[Set[String]] =
      ZIO.succeed(Set(""))

    override def toString = "ε"
  }

  /** A language that accepts lists of exactly one token. */
  sealed trait Token extends RegularLanguage {

    /** Returns `false`: a token needs one word. */
    def isNullable: Boolean = false
  }

  /**
   * A `StringToken(value)` language represents the regular language that contains only `value`. Unless
   * `CliConfig.caseSensitive` is `true`, `derive` matches `value` without case.
   */
  final case class StringToken(value: String) extends Token {

    /**
     * Returns `Epsilon` when `token` equals `value`, and `Empty` otherwise. The comparison ignores case unless
     * `cliConfig.caseSensitive` is `true`.
     */
    def derive(token: String, cliConfig: CliConfig) = {
      val isMatch = if (cliConfig.caseSensitive) value.equals(token) else value.equalsIgnoreCase(token)
      isMatch match {
        case true  => ZIO.succeed(Epsilon)
        case false => ZIO.succeed(Empty)
      }
    }

    /**
     * Returns `value` followed by a space when it starts with `prefix`, compared with case, and an empty set otherwise.
     */
    def firstTokens(prefix: String, compgen: Compgen): UIO[Set[String]] =
      ZIO.succeed(Set(value + " ").filter(_.startsWith(prefix)))
  }

  /**
   * `AnyStringToken` represents the set of all strings. For tab completion purposes, this is used to represent the name
   * of the executable. (It may be aliased or renamed to be different than `Command.Single.name`.)
   */
  case object AnyStringToken extends Token {

    /** Returns `Epsilon`, whatever `token` is. */
    def derive(token: String, cliConfig: CliConfig) = ZIO.succeed(Epsilon)

    /** Returns an empty set: there is no word to suggest. */
    def firstTokens(prefix: String, compgen: Compgen): UIO[Set[String]] =
      ZIO.succeed(Set.empty)
  }

  /**
   * A `PrimTypeToken(value)` language represents the regular language containing any strings `s` where
   * `value.validate(s)` succeeds.
   */
  final case class PrimTypeToken(value: PrimType[Any]) extends Token {

    /**
     * Returns `Epsilon` when `value.validate` accepts `token`, and `Empty` otherwise. For a `PrimType.Path`, validation
     * can read the file system.
     */
    def derive(token: String, cliConfig: CliConfig) =
      value
        .validate(token, cliConfig)
        .fold(
          _ => Empty,
          _ => Epsilon
        )

    /** Returns the completions that [[PrimTypeCompletion.firstTokens]] gives for `value` and `prefix`. */
    def firstTokens(prefix: String, compgen: Compgen): UIO[Set[String]] =
      PrimTypeCompletion.firstTokens(value, prefix, compgen)
  }

  /**
   * `Cat(left, right)` represents the concatenation of two regular languages.
   */
  final case class Cat(left: RegularLanguage, right: RegularLanguage) extends RegularLanguage {

    /** `true` when both `left` and `right` are nullable. */
    lazy val isNullable: Boolean = left.isNullable && right.isNullable

    /**
     * Returns the derivative of `left` followed by `right`. When `left` is nullable, the result also accepts the
     * derivative of `right`, because `token` can then start `right`.
     */
    def derive(token: String, cliConfig: CliConfig) =
      if (left.isNullable)
        left.derive(token, cliConfig).zip(right.derive(token, cliConfig)).map { case (dx, dy) =>
          (dx ~ right) | dy
        }
      else
        left.derive(token, cliConfig).map(dx => dx ~ right)

    /** Returns the first tokens of `left`, together with those of `right` when `left` is nullable. */
    def firstTokens(prefix: String, compgen: Compgen): UIO[Set[String]] =
      if (left.isNullable)
        left.firstTokens(prefix, compgen).zip(right.firstTokens(prefix, compgen)).map { case (left, right) =>
          left ++ right
        }
      else
        left.firstTokens(prefix, compgen)
  }

  /**
   * `Alt(left, right)` represents the union of two regular languages. We call it "Alt" for consistency with the names
   * used in the "Parsing With Derivatives" paper.
   */
  final case class Alt(left: RegularLanguage, right: RegularLanguage) extends RegularLanguage {

    /** `true` when `left` or `right` is nullable. */
    lazy val isNullable: Boolean = left.isNullable || right.isNullable

    /** Returns the union of the derivatives of `left` and `right`. */
    def derive(token: String, cliConfig: CliConfig) =
      left.derive(token, cliConfig).zip(right.derive(token, cliConfig)).map { case (dx, dy) => dx | dy }

    /** Returns the first tokens of `left` and `right` together. */
    def firstTokens(prefix: String, compgen: Compgen): UIO[Set[String]] =
      left.firstTokens(prefix, compgen).zip(right.firstTokens(prefix, compgen)).map { case (left, right) =>
        left ++ right
      }
  }

  /**
   * `Rep(language, min, max)` represents the repetition of `language`. The number of repetitions can be bounded via
   * `min` and `max`; `None` leaves that side unbounded. Setting both `min` and `max` to `None` represents the "Kleene
   * star" of `language`.
   */
  final case class Rep(language: RegularLanguage, min: Option[Int], max: Option[Int]) extends RegularLanguage {

    /** `true` when `min` is `None` or at most zero. */
    def isNullable: Boolean = min.forall(_ <= 0)

    /**
     * Returns the derivative of `language` followed by the repetition with `min` and `max` lowered by one, or `Empty`
     * when `max` is already zero or less.
     */
    def derive(token: String, cliConfig: CliConfig) = {
      val newMin = min.map(_ - 1).filter(_ > 0)
      val newMax = max.map(_ - 1)

      if (newMax.forall(_ >= 0))
        language.derive(token, cliConfig).map(dx => dx ~ language.rep(newMin, newMax))
      else
        ZIO.succeed(Empty)
    }

    /**
     * Returns the first tokens of `language`. The bounds are not read, so a repetition whose `max` is zero still
     * returns them.
     */
    def firstTokens(prefix: String, compgen: Compgen): UIO[Set[String]] =
      language.firstTokens(prefix, compgen)
  }

  /**
   * Permutation is like `Cat`, but the order of its parts does not matter: `Permutation(a1, a2, ..., ak)` accepts the
   * token lists of `a1` to `ak` in any order, each one once, and a nullable part can be left out. It is equivalent to
   * the following language:
   *
   * `a1 ~ Permutation(a2, a3, ..., ak) | a2 ~ Permutation(a1, a3, ..., ak) | ... | ak ~ Permutation(a1, a2, ..., ak-1)`
   *
   * So when we calculate its derivative, we apply the above "desugaring" transformation, then compute the derivative as
   * usual. Parts that are equal are removed together, so `Permutation(a, a)` accepts `a` once.
   *
   * @param values
   *   the parts, usually the languages of a command's options
   */
  final case class Permutation(values: RegularLanguage*) extends RegularLanguage {

    /** `true` when each part is nullable, and for a `Permutation` with no parts. */
    lazy val isNullable: Boolean = values.forall(_.isNullable)

    /**
     * This permutation as a union: `Epsilon`, and for each part, that part followed by a `Permutation` of the parts not
     * equal to it.
     */
    lazy val desugared: RegularLanguage =
      values.foldLeft[RegularLanguage](Epsilon)((agg, lang) => agg | (lang ~ Permutation(values.filter(_ != lang): _*)))

    /** Returns the derivative of `desugared`. */
    def derive(token: String, cliConfig: CliConfig) = desugared.derive(token, cliConfig)

    /** Returns the first tokens of each part together. */
    def firstTokens(prefix: String, compgen: Compgen): UIO[Set[String]] =
      ZIO.foreach(values)(lang => lang.firstTokens(prefix, compgen)).map(_.toSet.flatten)
  }
}
