package zio.cli

import zio._
import zio.test._
object ConfigFilesSpec extends ZIOSpecDefault {

  def spec = suite("ConfigFiles Suite")(
    suite("parseDotFile")(
      test("parse simple options") {
        val (args, sources) = ConfigFileResolver.parseDotFile("--verbose\n--output result.txt\n--count 42", "/f")
        assertTrue(
          args == List("--verbose", "--output", "result.txt", "--count", "42"),
          sources.length == 3
        )
      },
      test("parse key=value format") {
        val (args, _) = ConfigFileResolver.parseDotFile("--output=result.txt\n--count=42", "/f")
        assertTrue(args == List("--output=result.txt", "--count=42"))
      },
      test("skip empty lines and comments") {
        val content =
          """# This is a comment
            |--verbose
            |
            |# Another comment
            |--output result.txt
            |""".stripMargin
        val (args, _) = ConfigFileResolver.parseDotFile(content, "/f")
        assertTrue(args == List("--verbose", "--output", "result.txt"))
      },
      test("parse short options") {
        val (args, _) = ConfigFileResolver.parseDotFile("-v\n-o result.txt", "/f")
        assertTrue(args == List("-v", "-o", "result.txt"))
      },
      test("handle empty content") {
        val (args, sources) = ConfigFileResolver.parseDotFile("", "/f")
        assertTrue(args.isEmpty, sources.isEmpty)
      },
      test("handle quoted values") {
        val (args, _) = ConfigFileResolver.parseDotFile("--name \"John Doe\"", "/f")
        assertTrue(args == List("--name", "John Doe"))
      }
    ),
    suite("tokenizeLine")(
      test("simple tokens") {
        assertTrue(ConfigFileResolver.tokenizeLine("--name value") == List("--name", "value"))
      },
      test("double-quoted value") {
        assertTrue(ConfigFileResolver.tokenizeLine("--name \"hello world\"") == List("--name", "hello world"))
      },
      test("single-quoted value") {
        assertTrue(ConfigFileResolver.tokenizeLine("--name 'hello world'") == List("--name", "hello world"))
      },
      test("flag only") {
        assertTrue(ConfigFileResolver.tokenizeLine("--verbose") == List("--verbose"))
      },
      test("key=value") {
        assertTrue(ConfigFileResolver.tokenizeLine("--key=value") == List("--key=value"))
      }
    ),
    suite("mergeArgs")(
      test("CLI args override dotfile args") {
        val dotfileArgs    = List("--name", "from-dotfile", "--verbose")
        val dotfileSources = List(SettingSource("--name", "from-dotfile", "/f"), SettingSource("--verbose", "", "/f"))
        val cliArgs        = List("--name", "from-cli")

        val (merged, sources) = ConfigFileResolver.mergeArgs(dotfileArgs, dotfileSources, cliArgs)
        assertTrue(
          merged == List("--verbose", "--name", "from-cli"),
          sources.exists(s => s.name == "--name" && s.source == "<cli>"),
          sources.exists(s => s.name == "--verbose" && s.source == "/f")
        )
      },
      test("no dotfile args") {
        val (merged, sources) =
          ConfigFileResolver.mergeArgs(Nil, Nil, List("--name", "cli-value"))
        assertTrue(
          merged == List("--name", "cli-value"),
          sources.length == 1,
          sources.head.source == "<cli>"
        )
      },
      test("no CLI args - dotfile args pass through") {
        val dotfileArgs    = List("--name", "from-dotfile")
        val dotfileSources = List(SettingSource("--name", "from-dotfile", "/f"))

        val (merged, sources) = ConfigFileResolver.mergeArgs(dotfileArgs, dotfileSources, Nil)
        assertTrue(
          merged == List("--name", "from-dotfile"),
          sources.length == 1,
          sources.head.source == "/f"
        )
      },
      test("partial override: some from CLI, some from dotfile") {
        val dotfileArgs    = List("--name", "dot-name", "--output", "dot-out")
        val dotfileSources = List(
          SettingSource("--name", "dot-name", "/f"),
          SettingSource("--output", "dot-out", "/f")
        )
        val cliArgs = List("--name", "cli-name")

        val (merged, sources) = ConfigFileResolver.mergeArgs(dotfileArgs, dotfileSources, cliArgs)
        assertTrue(
          merged.contains("--output"),
          merged.contains("dot-out"),
          sources.exists(s => s.name == "--name" && s.source == "<cli>"),
          sources.exists(s => s.name == "--output" && s.source == "/f")
        )
      }
    ),
    suite("argsToOptionMap")(
      test("parse flag without value") {
        val result = ConfigFileResolver.argsToOptionMap(List("--verbose"))
        assertTrue(result == Map("--verbose" -> Nil))
      },
      test("parse option with value") {
        val result = ConfigFileResolver.argsToOptionMap(List("--output", "result.txt"))
        assertTrue(result == Map("--output" -> List("result.txt")))
      },
      test("parse multiple options") {
        val result = ConfigFileResolver.argsToOptionMap(List("--verbose", "--output", "result.txt"))
        assertTrue(
          result == Map("--verbose" -> Nil, "--output" -> List("result.txt"))
        )
      },
      test("handle empty list") {
        assertTrue(ConfigFileResolver.argsToOptionMap(Nil) == Map.empty[String, List[String]])
      }
    ),
    suite("formatDiagnostics")(
      test("formats sources correctly") {
        val sources = List(
          SettingSource("--name", "value", "/home/.myapp"),
          SettingSource("--verbose", "", "<cli>")
        )
        val result = ConfigFileResolver.formatDiagnostics(sources)
        assertTrue(
          result.contains("--name"),
          result.contains("/home/.myapp"),
          result.contains("--verbose"),
          result.contains("<cli>")
        )
      },
      test("empty sources") {
        assertTrue(ConfigFileResolver.formatDiagnostics(Nil) == "No settings loaded.")
      }
    ),
    suite("end-to-end integration with Options and Command")(
      test("dotfile args merged into command parsing") {
        val command  = Command("test", Options.text("name"), Args.none)
        val resolver = new ConfigFileResolver {
          def resolve(appName: String): UIO[(List[String], List[SettingSource])] =
            ZIO.succeed(
              (List("--name", "from-dotfile"), List(SettingSource("--name", "from-dotfile", "/test/.testrc")))
            )
        }

        val app = CliApp.make[Any, Nothing, String, String](
          name = "test",
          version = "1.0",
          summary = HelpDoc.Span.text("test app"),
          command = command,
          configFileResolver = resolver
        ) { name =>
          ZIO.succeed(name)
        }

        for {
          // Without CLI override - uses dotfile value
          r1 <- app.run(Nil)
          // With CLI override
          r2 <- app.run(List("--name", "from-cli"))
        } yield assertTrue(
          r1 == Some("from-dotfile"),
          r2 == Some("from-cli")
        )
      },
      test("boolean flag from dotfile") {
        val command  = Command("test", Options.boolean("verbose", true) ++ Options.text("name"), Args.none)
        val resolver = new ConfigFileResolver {
          def resolve(appName: String): UIO[(List[String], List[SettingSource])] =
            ZIO.succeed(
              (List("--verbose"), List(SettingSource("--verbose", "", "/test/.testrc")))
            )
        }

        val app = CliApp.make[Any, Nothing, (Boolean, String), (Boolean, String)](
          name = "test",
          version = "1.0",
          summary = HelpDoc.Span.text("test"),
          command = command,
          configFileResolver = resolver
        ) { case (verbose, name) =>
          ZIO.succeed((verbose, name))
        }

        app.run(List("--name", "hello")).map { result =>
          assertTrue(result == Some((true, "hello")))
        }
      },
      test("no resolver returns defaults") {
        val command = Command("test", Options.text("name").withDefault("default-val"), Args.none)

        val app = CliApp.make[Any, Nothing, String, String](
          name = "test",
          version = "1.0",
          summary = HelpDoc.Span.text("test"),
          command = command,
          configFileResolver = ConfigFileResolver.none
        ) { name =>
          ZIO.succeed(name)
        }

        app.run(Nil).map(r => assertTrue(r == Some("default-val")))
      }
    )
  )
}
