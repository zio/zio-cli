package zio.cli

package object completion {

  /**
   * `StringExtensions` contains a few handy operators for creating `RegularLanguage` values from strings.
   *
   * @param self
   *   a string value that will be treated as a `RegularLanguage.StringToken`.
   */
  implicit class StringExtensions(self: String) {

    /** Returns the word `self` followed by the word `other`: `"git" ~ "add"` accepts `List("git", "add")`. */
    def ~(other: String): RegularLanguage = RegularLanguage.StringToken(self) ~ other

    /** Returns the language that accepts either the word `self` or the word `other`. */
    def |(other: String): RegularLanguage = RegularLanguage.StringToken(self) | other
  }
}
