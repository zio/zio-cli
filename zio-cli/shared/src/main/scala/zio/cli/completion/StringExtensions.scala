package zio.cli

package object completion {

  /**
   * `StringExtensions` contains a few handy operators for creating
   * `RegularLanguage` values from strings.
   *
   * @param self a string value that will be treated as a `RegularLanguage.StringToken`.
   */
  implicit class StringExtensions(self: String) {
    def ~(other: String): RegularLanguage = RegularLanguage.StringToken(self) ~ other
    def |(other: String): RegularLanguage = RegularLanguage.StringToken(self) | other
  }
}
