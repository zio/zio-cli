package zio.cli.completion

import zio.test.Assertion._
import zio.test._
import zio.cli._

object RegularLanguageSpec extends ZIOSpecDefault {
  def spec = suite("RegularLanguage Spec")(
    suite("Toplevel Command Completion Spec")(
      suite("Empty language")(
        test("Empty language rejects all strings")(
          check(Gen.listOfBounded(0, 5)(Gen.alphaNumericString))(tokens =>
            assertM(
              RegularLanguage.Empty.contains(tokens)
            )(equalTo(false)).provideService(CliConfig.default)
          )
        )
      ),
      suite("Epsilon language")(
        test("Epsilon language accepts the empty string")(
          assertM(
            RegularLanguage.Epsilon.contains(List.empty)
          )(equalTo(true)).provideService(CliConfig.default)
        ),
        test("Epsilon language rejects all nonempty strings")(
          check(Gen.listOfBounded(1, 5)(Gen.alphaNumericString))(tokens =>
            assertM(
              RegularLanguage.Empty.contains(tokens)
            )(equalTo(false)).provideService(CliConfig.default)
          )
        )
      ),
      suite("StringToken language")(
        test("StringToken language accepts its target string")(
          assertM(
            RegularLanguage.StringToken("foo").contains(List("foo"))
          )(equalTo(true)).provideService(CliConfig.default)
        ),
        test("StringToken language rejects anything other than its target string")(
          check(Gen.alphaNumericString.filter(_ != "foo"))(token =>
            assertM(
              RegularLanguage.StringToken("foo").contains(List(token))
            )(equalTo(false)).provideService(CliConfig.default)
          )
        )
      ),
      suite("PrimTypeToken language")(
        suite("PrimType.Bool language")(
          test("PrimType.Bool language accepts values that correspond to 'true' and 'false' ")(
            check(Gen.fromIterable(PrimType.Bool.TrueValues ++ PrimType.Bool.FalseValues))(token =>
              assertM(
                RegularLanguage.PrimTypeToken(PrimType.Bool(None)).contains(List(token))
              )(equalTo(true)).provideService(CliConfig.default)
            )
          ),
          test("PrimType.Bool language rejects values that do not correspond to 'true'/'false'")(
            check(
              Gen.alphaNumericString
                .map(s => s.toLowerCase)
                .filter(s => !PrimType.Bool.TrueValues(s) && !PrimType.Bool.FalseValues(s))
            )(token =>
              assertM(
                RegularLanguage.PrimTypeToken(PrimType.Bool(None)).contains(List(token))
              )(equalTo(false)).provideService(CliConfig.default)
            )
          )
        ),
        suite("PrimType.ZoneId language")(
          test("PrimType.ZoneId language accepts some that are valid time zone IDs.")(
            check(
              Gen.fromIterable(
                Iterable(
                  "US/Alaska",
                  "US/Aleutian",
                  "US/Arizona",
                  "US/Central",
                  "US/East-Indiana",
                  "US/Eastern",
                  "US/Hawaii",
                  "US/Indiana-Starke",
                  "US/Michigan",
                  "US/Mountain",
                  "US/Pacific",
                  "US/Samoa"
                )
              )
            )(token =>
              assertM(
                RegularLanguage.PrimTypeToken(PrimType.ZoneId).contains(List(token))
              )(equalTo(true)).provideService(CliConfig.default)
            )
          ),
          test("PrimType.Bool language rejects some values that are not valid time zone IDs")(
            check(
              Gen.fromIterable(Iterable("invalid", "whatever", "foo"))
            )(token =>
              assertM(
                RegularLanguage.PrimTypeToken(PrimType.ZoneId).contains(List(token))
              )(equalTo(false)).provideService(CliConfig.default)
            )
          )
        )
      ),
      suite("Cat language")(
        suite("'foo' 'bar' 'baz' language")(
          test("Accepts 'foo' 'bar' 'baz'")(
            assertM(
              ("foo" ~ "bar" ~ "baz")
                .contains(List("foo", "bar", "baz"))
            )(equalTo(true)).provideService(CliConfig.default)
          ),
          test("Rejects everything that is not 'foo' 'bar' 'baz'")(
            check(
              Gen.fromIterable(
                Iterable(
                  List(),
                  List("foo"),
                  List("foo", "bar"),
                  List("bar", "baz"),
                  List("foo", "bar", "baz", "bippy")
                )
              )
            )(tokens =>
              assertM(
                ("foo" ~ "bar" ~ "baz")
                  .contains(tokens)
              )(equalTo(false)).provideService(CliConfig.default)
            )
          )
        )
      ),
      suite("Alt language")(
        suite("'foo' 'bar' | 'foo' 'baz' language")(
          test("Accepts 'foo' 'bar'")(
            assertM(
              ("foo" ~ "bar" | "foo" ~ "baz")
                .contains(List("foo", "bar"))
            )(equalTo(true)).provideService(CliConfig.default)
          ),
          test("Accepts 'foo' 'baz'")(
            assertM(
              ("foo" ~ "bar" | "foo" ~ "baz")
                .contains(List("foo", "baz"))
            )(equalTo(true)).provideService(CliConfig.default)
          ),
          test("Rejects everything that is not 'foo' 'bar' | 'foo' 'baz'")(
            check(
              Gen.fromIterable(
                Iterable(
                  List(),
                  List("foo"),
                  List("foo", "bar", "baz"),
                  List("foo", "bar", "foo", "baz")
                )
              )
            )(tokens =>
              assertM(
                ("foo" ~ "bar" | "foo" ~ "baz")
                  .contains(tokens)
              )(equalTo(false)).provideService(CliConfig.default)
            )
          )
        )
      ),
      suite("Rep language")(
        suite("('foo' 'bar' | 'foo' 'baz')* language")(
          test("Accepts zero or more repetitions of 'foo' 'bar' or 'foo' 'baz'")(
            check(
              Gen.fromIterable(
                Iterable(
                  List(),
                  List("foo", "bar"),
                  List("foo", "baz"),
                  List("foo", "bar", "foo", "baz"),
                  List("foo", "baz", "foo", "bar"),
                  List("foo", "bar", "foo", "bar"),
                  List("foo", "baz", "foo", "baz"),
                  List("foo", "bar", "foo", "baz", "foo", "bar"),
                  List("foo", "baz", "foo", "baz", "foo", "bar", "foo", "baz")
                )
              )
            )(tokens =>
              assertM(
                ("foo" ~ "bar" | "foo" ~ "baz").*.contains(tokens)
              )(equalTo(true)).provideService(CliConfig.default)
            )
          ),
          test("Rejects everything except zero or more repetitions of 'foo' 'bar' or 'foo' 'baz'")(
            check(
              Gen.fromIterable(
                Iterable(
                  List("foo", "bar", "foo"),
                  List("foo", "baz", "foo"),
                  List("foo", "bar", "foo", "baz", "foo"),
                  List("foo", "baz", "foo", "bar", "baz"),
                  List("foo", "bar", "foo", "bar", "bar"),
                  List("foo", "baz", "foo", "baz", "baz")
                )
              )
            )(tokens =>
              assertM(
                ("foo" ~ "bar" | "foo" ~ "baz").*.contains(tokens)
              )(equalTo(false)).provideService(CliConfig.default)
            )
          )
        ),
        suite("('foo' 'bar' | 'foo' 'baz').rep(2, 4) language")(
          test("Accepts two to four or more repetitions of 'foo' 'bar' or 'foo' 'baz'")(
            check(
              Gen.fromIterable(
                Iterable(
                  List("foo", "bar", "foo", "bar"),
                  List("foo", "bar", "foo", "baz"),
                  List("foo", "bar", "foo", "baz", "foo", "bar"),
                  List("foo", "baz", "foo", "bar", "foo", "bar"),
                  List("foo", "bar", "foo", "baz", "foo", "bar", "foo", "baz"),
                  List("foo", "baz", "foo", "baz", "foo", "bar", "foo", "baz")
                )
              )
            )(tokens =>
              assertM(
                ("foo" ~ "bar" | "foo" ~ "baz").rep(Some(2), Some(4)).contains(tokens)
              )(equalTo(true)).provideService(CliConfig.default)
            )
          ),
          test("Rejects everything except two to four or more repetitions of 'foo' 'bar' or 'foo' 'baz'")(
            check(
              Gen.fromIterable(
                Iterable(
                  List(),
                  List("foo", "bar"),
                  List("foo", "baz"),
                  List("foo", "baz", "foo"),
                  List("foo", "baz", "bar"),
                  List("foo", "bar", "foo", "baz", "foo"),
                  List("foo", "baz", "foo", "bar", "baz"),
                  List("foo", "bar", "foo", "bar", "bar"),
                  List("foo", "baz", "foo", "baz", "baz"),
                  List("foo", "baz", "foo", "baz", "foo", "baz", "foo", "baz", "foo", "baz")
                )
              )
            )(tokens =>
              assertM(
                ("foo" ~ "bar" | "foo" ~ "baz").rep(Some(2), Some(4)).contains(tokens)
              )(equalTo(false)).provideService(CliConfig.default)
            )
          )
        )
      ),
      suite("Permutation language")(
        suite("Permutation('a', 'b', 'c', 'd') language")(
          test("Accepts permutations of {'a', 'b', 'c', 'd'}")(
            check(
              Gen.fromIterable(List("a", "b", "c", "d").permutations.toList)
            )(tokens =>
              assertM(
                RegularLanguage
                  .Permutation(
                    List("a", "b", "c", "d").map(RegularLanguage.StringToken(_)): _*
                  )
                  .contains(tokens)
              )(equalTo(true)).provideService(CliConfig.default)
            )
          ),
          test("Rejects everything except for permutations of {'a', 'b', 'c', 'd'}")(
            check(
              Gen.fromIterable(
                Iterable(
                  List(),
                  List("a"),
                  List("b"),
                  List("c"),
                  List("d"),
                  List("a", "b", "c"),
                  List("d", "c", "b"),
                  List("a", "b", "c", "d", "d")
                )
              )
            )(tokens =>
              assertM(
                RegularLanguage
                  .Permutation(
                    List("a", "b", "c", "d").map(RegularLanguage.StringToken(_)): _*
                  )
                  .contains(tokens)
              )(equalTo(false)).provideService(CliConfig.default)
            )
          )
        ),
        suite("Permutation('a', 'b' | 'c', 'd'.*) language")(
          test("Accepts language members")(
            check(
              Gen.fromIterable(
                Iterable(
                  List("a", "b"),
                  List("a", "c"),
                  List("a", "b", "d"),
                  List("a", "b", "d", "d", "d"),
                  List("a", "c", "d"),
                  List("a", "c", "d", "d", "d"),
                  List("d", "b", "a"),
                  List("d", "d", "d", "b", "a"),
                  List("d", "c", "a"),
                  List("d", "d", "d", "c", "a"),
                  List("d", "a", "b")
                )
              )
            )(tokens =>
              assertM(
                RegularLanguage
                  .Permutation(
                    RegularLanguage.StringToken("a"),
                    "b" | "c",
                    RegularLanguage.StringToken("d").*
                  )
                  .contains(tokens)
              )(equalTo(true)).provideService(CliConfig.default)
            )
          ),
          test("Rejects language non-members")(
            check(
              Gen.fromIterable(
                Iterable(
                  List(),
                  List("a"),
                  List("b"),
                  List("c"),
                  List("d"),
                  List("d", "a", "d"),
                  List("a", "c", "c")
                )
              )
            )(tokens =>
              assertM(
                RegularLanguage
                  .Permutation(
                    RegularLanguage.StringToken("a"),
                    "b" | "c",
                    RegularLanguage.StringToken("d").*
                  )
                  .contains(tokens)
              )(equalTo(false)).provideService(CliConfig.default)
            )
          )
        ),
        suite("Permutation('a'? 'b'? 'c'? 'd'?) ~ 'z' language")(
          test("Accepts language members")(
            check(
              Gen.fromIterable(
                Iterable(
                  List("z"),
                  List("a", "b", "z"),
                  List("a", "c", "z"),
                  List("a", "b", "d", "z"),
                  List("d", "z"),
                  List("a", "c", "d", "z"),
                  List("d", "b", "a", "z"),
                  List("d", "c", "a", "z"),
                  List("d", "a", "b", "z")
                )
              )
            )(tokens =>
              assertM(
                (
                  RegularLanguage
                    .Permutation(
                      RegularLanguage.StringToken("a").?,
                      RegularLanguage.StringToken("b").?,
                      RegularLanguage.StringToken("c").?,
                      RegularLanguage.StringToken("d").?
                    ) ~ "z"
                ).contains(tokens)
              )(equalTo(true)).provideService(CliConfig.default)
            )
          ),
          test("Rejects language non-members")(
            check(
              Gen.fromIterable(
                Iterable(
                  List(),
                  List("a", "b"),
                  List("a", "c", "c", "z"),
                  List("a", "b", "d"),
                  List("d", "z", "z"),
                  List("a", "c", "d"),
                  List("d", "b", "a"),
                  List("d", "c", "a"),
                  List("d", "a", "b")
                )
              )
            )(tokens =>
              assertM(
                (
                  RegularLanguage
                    .Permutation(
                      RegularLanguage.StringToken("a").?,
                      RegularLanguage.StringToken("b").?,
                      RegularLanguage.StringToken("c").?,
                      RegularLanguage.StringToken("d").?
                    ) ~ "z"
                ).contains(tokens)
              )(equalTo(false)).provideService(CliConfig.default)
            )
          )
        )
      )
    )
  )
}
