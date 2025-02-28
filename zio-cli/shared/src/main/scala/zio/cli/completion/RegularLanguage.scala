package zio.cli.completion

import zio._
import zio.cli.CliConfig
import zio.cli.PrimType

/**
 * `RegularLanguage` is an implementation of "Parsing With Derivatives" (Might et al. 2011) that is used for CLI tab
 * completion. Unlike your usual regular languages that are sets of strings of symbols, our regular languages are sets
 * of lists of tokens, where tokens can be strings or `zio.cli.PrimType` instances. (If you think about it,
 * `zio.cli.PrimType.validate` is an intensional definition of a set of strings.)
 */
sealed trait RegularLanguage extends Product with Serializable {
  import RegularLanguage._

  /**
   * This is the delta (δ) predicate from "Parsing With Derivatives", indicating whether this language contains the
   * empty string.
   *
   * @return
   *   true if and only if this language contains the empty string.
   */
  def isNullable: Boolean

  /**
   * Calculate the Brzozowski derivative of this language with respect to the given string. This is an effectful
   * function because it can call PrimType.validate (e.g., when validating file paths, etc.).
   *
   * @param token
   *   The string to use for calculation of the Brzozowski derivative.
   * @return
   *   Brzozowski derivative wrapped in an UIO instance.
   */
  def derive(token: String, cliConfig: CliConfig): UIO[RegularLanguage]

  def ~(other: RegularLanguage): RegularLanguage                             = Cat(this, other)
  def ~(other: String): RegularLanguage                                      = Cat(this, StringToken(other))
  def |(other: RegularLanguage): RegularLanguage                             = Alt(this, other)
  def |(other: String): RegularLanguage                                      = Alt(this, StringToken(other))
  def *                                                                      = Rep(this, None, None): RegularLanguage
  def rep(min: Option[Int] = None, max: Option[Int] = None): RegularLanguage = Rep(this, min, max)
  def ?                                                                      = Alt(this, Epsilon)

  /**
   * Checks to see if the input token list is a member of the language.
   *
   * @param tokens
   * @return
   *   true if and only if `tokens` is in the language.
   */
  def contains(tokens: List[String], cliConfig: CliConfig): UIO[Boolean] =
    ZIO
      .foldLeft(tokens)(this)((language, word) => language.derive(word, cliConfig))
      .map(dx => dx.isNullable)

  /**
   * Returns a set consisting of the first token of all strings in this language that are useful for CLI tab completion.
   * For infinite or unwieldly languages, it is perfectly fine to return the empty set: This will simply not display any
   * completions to the user.
   *
   * If you'd like the cursor to advance to the next word when tab completion unambiguously matches the prefix to a
   * token, append a space (" ") character to the end of the returned token. Otherwise, the cursor will skip to the end
   * of the completed token in the terminal.
   *
   * Some examples of different use cases:
   *   1. Completing file/directory names:
   *      - Append a space to the ends of file names (e.g., "bippy.pdf"). This is because we want the cursor to jump to
   *        the next argument position if tab completion unambiguously succeeds.
   *
   *   - Do not append a space to the end of a directory name (e.g., "foo/"). This is because we want the user to be
   *     able to press tab again to gradually complete a lengthy file path.
   *   - Append a space to the ends of string tokens.
   *
   * You may be asking why we don't try to use the `-o nospace` setting of `compgen` and `complete`. The answer is they
   * appear to be all or nothing: For a given tab completion execution, you have to choose one behavior or the other.
   * This does not work well when completing both file names and directory names at the same time.
   */
  def firstTokens(prefix: String, compgen: Compgen): UIO[Set[String]]
}

object RegularLanguage {

  /**
   * The `Empty` language (∅) accepts no strings.
   */
  case object Empty extends RegularLanguage {
    def isNullable: Boolean = false

    def derive(token: String, cliConfig: CliConfig) = ZIO.succeed(Empty)

    def firstTokens(prefix: String, compgen: Compgen): UIO[Set[String]] =
      ZIO.succeed(Set.empty)

    override def toString = "∅"
  }

  /**
   * The `Epsilon` language (ε) accepts only the empty string.
   */
  case object Epsilon extends RegularLanguage {
    def isNullable: Boolean = true

    def derive(token: String, cliConfig: CliConfig) = ZIO.succeed(Empty)

    def firstTokens(prefix: String, compgen: Compgen): UIO[Set[String]] =
      ZIO.succeed(Set(""))

    override def toString = "ε"
  }

  sealed trait Token extends RegularLanguage {
    def isNullable: Boolean = false
  }

  /**
   * A `StringToken(value)` language represents the regular language that contains only `value`.
   */
  final case class StringToken(value: String) extends Token {
    def derive(token: String, cliConfig: CliConfig) = {
      val isMatch = if (cliConfig.caseSensitive) value.equals(token) else value.equalsIgnoreCase(token)
      isMatch match {
        case true  => ZIO.succeed(Epsilon)
        case false => ZIO.succeed(Empty)
      }
    }

    def firstTokens(prefix: String, compgen: Compgen): UIO[Set[String]] =
      ZIO.succeed(Set(value + " ").filter(_.startsWith(prefix)))
  }

  /**
   * `AnyStringToken` represents the set of all strings. For tab completion purposes, this is used to represent the name
   * of the executable. (It may be aliased or renamed to be different than `Command.Single.name`.)
   */
  case object AnyStringToken extends Token {
    def derive(token: String, cliConfig: CliConfig) = ZIO.succeed(Epsilon)

    def firstTokens(prefix: String, compgen: Compgen): UIO[Set[String]] =
      ZIO.succeed(Set.empty)
  }

  /**
   * A `PrimTypeToken(value)` language represents the regular language containing any strings `s` where
   * `value.validate(s)` succeeds.
   */
  final case class PrimTypeToken(value: PrimType[Any]) extends Token {
    def derive(token: String, cliConfig: CliConfig) =
      value
        .validate(token, cliConfig)
        .fold(
          _ => Empty,
          _ => Epsilon
        )

    def firstTokens(prefix: String, compgen: Compgen): UIO[Set[String]] =
      PrimTypeCompletion.firstTokens(value, prefix, compgen)
  }

  /**
   * `Cat(left, right)` represents the concatenation of two regular languages.
   */
  final case class Cat(left: RegularLanguage, right: RegularLanguage) extends RegularLanguage {
    lazy val isNullable: Boolean = left.isNullable && right.isNullable

    def derive(token: String, cliConfig: CliConfig) =
      if (left.isNullable)
        left.derive(token, cliConfig).zip(right.derive(token, cliConfig)).map { case (dx, dy) =>
          (dx ~ right) | dy
        }
      else
        left.derive(token, cliConfig).map(dx => dx ~ right)

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
    lazy val isNullable: Boolean = left.isNullable || right.isNullable

    def derive(token: String, cliConfig: CliConfig) =
      left.derive(token, cliConfig).zip(right.derive(token, cliConfig)).map { case (dx, dy) => dx | dy }

    def firstTokens(prefix: String, compgen: Compgen): UIO[Set[String]] =
      left.firstTokens(prefix, compgen).zip(right.firstTokens(prefix, compgen)).map { case (left, right) =>
        left ++ right
      }
  }

  /**
   * `Rep(language, min, max)` represents the repetition of `language`. The number of repetitions can be bounded via
   * `min` and `max`. Setting `max=None` represents the "Kleene star" of `language`.
   */
  final case class Rep(language: RegularLanguage, min: Option[Int], max: Option[Int]) extends RegularLanguage {
    def isNullable: Boolean = min.forall(_ <= 0)

    def derive(token: String, cliConfig: CliConfig) = {
      val newMin = min.map(_ - 1).filter(_ > 0)
      val newMax = max.map(_ - 1)

      if (newMax.forall(_ >= 0))
        language.derive(token, cliConfig).map(dx => dx ~ language.rep(newMin, newMax))
      else
        ZIO.succeed(Empty)
    }

    def firstTokens(prefix: String, compgen: Compgen): UIO[Set[String]] =
      language.firstTokens(prefix, compgen)
  }

  /**
   * Permutation is like `Cat`, but it is a commutative monoid. A Permutation(a_1, a_2, ..., a_{k}) is equivalent to the
   * following language:
   *
   * a2 ~ Permutation(a_1, a_3, ..., a_k) | a1 ~ Permutation(a_2, a_3, ..., a_k) | ... ak ~ Permutation(a_1, a_2, ...,
   * a_{k - 1})
   *
   * So when we calculate its derivative, we apply the above "desugaring" transformation, then compute the derivative as
   * usual.
   *
   * @param values
   */
  final case class Permutation(values: RegularLanguage*) extends RegularLanguage {
    lazy val isNullable: Boolean = values.forall(_.isNullable)

    lazy val desugared: RegularLanguage =
      values.foldLeft[RegularLanguage](Epsilon)((agg, lang) => agg | (lang ~ Permutation(values.filter(_ != lang): _*)))

    def derive(token: String, cliConfig: CliConfig) = desugared.derive(token, cliConfig)

    def firstTokens(prefix: String, compgen: Compgen): UIO[Set[String]] =
      ZIO.foreach(values)(lang => lang.firstTokens(prefix, compgen)).map(_.toSet.flatten)
  }
}
