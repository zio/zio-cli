package zio.cli.config

import zio.Scope
import zio.test.Assertion._
import zio.test._

object ConfigParserMergerSpec extends ZIOSpecDefault {

  override def spec: Spec[TestEnvironment with Scope, Any] =
    suite("ConfigParserMergerSpec")(
      test("parse --key=value") {
        val parsed = ConfigParser.parseLines(List("--line-ending=\\n"), "/tmp/.wc", priority = 0)
        assert(parsed)(
          equalTo(List(ConfigOption("--line-ending", List("--line-ending=\\n"), "/tmp/.wc", 0)))
        )
      },
      test("parse --key value and preserve spaces inside quotes") {
        val parsed = ConfigParser.parseLines(List("--name \"john doe\""), "/tmp/.wc", priority = 1)
        assert(parsed)(
          equalTo(List(ConfigOption("--name", List("--name", "john doe"), "/tmp/.wc", 1)))
        )
      },
      test("ignore comments and invalid lines") {
        val parsed = ConfigParser.parseLines(
          List("# comment", "", "line-ending=lf", "--ok"),
          "/tmp/.wc",
          priority = 2
        )

        assert(parsed)(equalTo(List(ConfigOption("--ok", List("--ok"), "/tmp/.wc", 2))))
      },
      test("higher-priority files override lower-priority files") {
        val options = List(
          ConfigOption("--count", List("--count=1"), "/home/.wc", priority = 0),
          ConfigOption("--count", List("--count=2"), "/work/.wc", priority = 1),
          ConfigOption("--verbose", List("--verbose"), "/home/.wc", priority = 0)
        )

        val (merged, diagnostics) = ConfigMerger.mergeWithDiagnostics(options, Nil)

        assert(merged.toSet)(equalTo(Set("--count=2", "--verbose"))) &&
        assert(diagnostics.resolvedOptions.map(_.key).toSet)(equalTo(Set("--count", "--verbose")))
      },
      test("CLI args override file options") {
        val options = List(
          ConfigOption("--count", List("--count=1"), "/home/.wc", priority = 0),
          ConfigOption("--verbose", List("--verbose"), "/home/.wc", priority = 0)
        )

        val (merged, diagnostics) = ConfigMerger.mergeWithDiagnostics(options, List("--count", "3"))

        assert(merged)(equalTo(List("--verbose", "--count", "3"))) &&
        assert(diagnostics.cliOverrides)(contains("--count"))
      },
      test("CLI args are not reported as overrides when no file options are present") {
        val (merged, diagnostics) = ConfigMerger.mergeWithDiagnostics(Nil, List("-a", "A", "-b", "B"))

        assert(merged)(equalTo(List("-a", "A", "-b", "B"))) &&
        assert(diagnostics.cliOverrides)(isEmpty) &&
        assert(diagnostics.conflicts)(isEmpty)
      }
    )
}
