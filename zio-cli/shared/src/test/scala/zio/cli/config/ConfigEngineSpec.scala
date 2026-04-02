package zio.cli.config

import zio.test._
import zio.test.Assertion._

object ConfigEngineSpec extends ZIOSpecDefault {

  def spec = suite("ConfigEngineSpec")(
    suite("ConfigFileResolver")(
      test("1. resolves without error on a nonexistent command") {
        for {
          opts <- ConfigFileResolver.resolveAndParse("nonexistent_xyz_test")
        } yield assert(opts)(isEmpty)
      },
      test("2. resolves without error when command name is valid") {
        for {
          opts <- ConfigFileResolver.resolveAndParse("testcli_check")
        } yield assert(opts)(isEmpty)
      }
    ),
    suite("ConfigParser")(
      test("3. parses --key=value correctly") {
        val parsed = ConfigParser.parseLine("--max-lines=100", "/path/.wc", 1)
        assert(parsed)(isSome(equalTo(ConfigOption("--max-lines", Some("100"), "/path/.wc", 1))))
      },
      test("4. parses --flag correctly") {
        val parsed = ConfigParser.parseLine("--verbose", "/path/.wc", 2)
        assert(parsed)(isSome(equalTo(ConfigOption("--verbose", None, "/path/.wc", 2))))
      },
      test("5. ignores invalid lines without dashes") {
        val parsed = ConfigParser.parseLine("invalid-line-without-dash", "/src", 1)
        assert(parsed)(isNone)
      },
      test("6. ignores empty lines") {
        val parsed = ConfigParser.parseLine("", "/src", 1)
        assert(parsed)(isNone)
      },
      test("7. handles trailing spaces on flags") {
        val parsed = ConfigParser.parseLine("--flag  ", "/src", 1)
        assert(parsed)(isSome(equalTo(ConfigOption("--flag", None, "/src", 1))))
      },
      test("8. ignores comment lines") {
        val lines = List("# this is a comment", "--flag", "# another comment")
        val opts  = ConfigParser.parseLines(lines, "/src", 0)
        assert(opts.length)(equalTo(1)) &&
        assert(opts.head.key)(equalTo("--flag"))
      },
      test("9. parses multiple lines correctly") {
        val lines = List("--a=1", "--b", "--c=hello")
        val opts  = ConfigParser.parseLines(lines, "/src", 0)
        assert(opts.length)(equalTo(3))
      },
      test("10. parses short flags") {
        val parsed = ConfigParser.parseLine("-v", "/src", 0)
        assert(parsed)(isSome(equalTo(ConfigOption("-v", None, "/src", 0))))
      }
    ),
    suite("ConfigMerger")(
      test("11. CLI args override file options") {
        val fileOpts       = List(ConfigOption("--max", Some("10"), "a", 1))
        val cliArgs        = List("--max=20")
        val (merged, diag) = ConfigMerger.mergeWithDiagnostics(fileOpts, cliArgs)
        assert(merged)(equalTo(List("--max=20"))) &&
        assert(diag.cliOverrides)(contains("--max"))
      },
      test("12. handles separated CLI overrides") {
        val fileOpts       = List(ConfigOption("--max", Some("10"), "a", 1))
        val cliArgs        = List("--max", "20")
        val (merged, diag) = ConfigMerger.mergeWithDiagnostics(fileOpts, cliArgs)
        assert(merged)(equalTo(List("--max", "20"))) &&
        assert(diag.cliOverrides)(contains("--max"))
      },
      test("13. higher priority file overrides lower") {
        val fileOpts = List(
          ConfigOption("--max", Some("10"), "lower", 1),
          ConfigOption("--max", Some("50"), "higher", 2)
        )
        val (merged, diag) = ConfigMerger.mergeWithDiagnostics(fileOpts, List.empty[String])
        assert(merged)(equalTo(List("--max=50"))) &&
        assert(diag.conflicts.head.winner.get.priority)(equalTo(2))
      },
      test("14. removes duplicate flags across files") {
        val fileOpts = List(
          ConfigOption("--flag", None, "1", 1),
          ConfigOption("--flag", None, "2", 2),
          ConfigOption("--flag", None, "3", 3)
        )
        val (merged, diag) = ConfigMerger.mergeWithDiagnostics(fileOpts, List.empty)
        assert(merged)(equalTo(List("--flag"))) &&
        assert(diag.conflicts.head.overridden.length)(equalTo(2))
      },
      test("15. merges disjoint keys properly") {
        val fileOpts = List(
          ConfigOption("--a", Some("1"), "1", 1),
          ConfigOption("--b", Some("2"), "2", 2)
        )
        val (merged, _) = ConfigMerger.mergeWithDiagnostics(fileOpts, List("--c", "3"))
        assert(merged.toSet)(equalTo(Set("--a=1", "--b=2", "--c", "3")))
      },
      test("16. records cli overrides properly in diagnostics") {
        val fileOpts  = List(ConfigOption("-f", None, "1", 1))
        val (_, diag) = ConfigMerger.mergeWithDiagnostics(fileOpts, List("-f"))
        assert(diag.cliOverrides)(equalTo(List("-f"))) &&
        assert(diag.conflicts.head.cliOverride)(isTrue)
      },
      test("17. diagnostics resolvedOptions captured correctly") {
        val fileOpts = List(
          ConfigOption("-a", None, "1", 1),
          ConfigOption("-b", None, "2", 2)
        )
        val (_, diag) = ConfigMerger.mergeWithDiagnostics(fileOpts, List.empty)
        assert(diag.resolvedOptions.map(_.key).toSet)(equalTo(Set("-a", "-b")))
      },
      test("18. merges key without value but cli has value") {
        val fileOpts       = List(ConfigOption("--k", None, "1", 1))
        val (merged, diag) = ConfigMerger.mergeWithDiagnostics(fileOpts, List("--k=10"))
        assert(merged)(equalTo(List("--k=10"))) &&
        assert(diag.cliOverrides)(contains("--k"))
      },
      test("19. respects original cli args order") {
        val fileOpts    = List(ConfigOption("--fromFile", None, "1", 1))
        val (merged, _) = ConfigMerger.mergeWithDiagnostics(fileOpts, List("arg1", "arg2"))
        assert(merged.takeRight(2))(equalTo(List("arg1", "arg2")))
      },
      test("20. does not falsely flag unique key as overridden") {
        val fileOpts  = List(ConfigOption("--x", Some("10"), "1", 1))
        val (_, diag) = ConfigMerger.mergeWithDiagnostics(fileOpts, List("--y", "20"))
        assert(diag.conflicts)(isEmpty)
      },
      test("21. empty file options means CLI args pass through unchanged") {
        val (merged, _) = ConfigMerger.mergeWithDiagnostics(Nil, List("--a", "--b"))
        assert(merged)(equalTo(List("--a", "--b")))
      }
    )
  )
}
