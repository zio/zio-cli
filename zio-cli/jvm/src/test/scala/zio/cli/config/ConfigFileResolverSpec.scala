package zio.cli.config

import zio._
import zio.cli._
import zio.test.Assertion._
import zio.test._

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

object ConfigFileResolverSpec extends ZIOSpecDefault {

  private def writeFile(path: Path, lines: List[String]): Task[Unit] =
    ZIO.attempt {
      Files.createDirectories(path.getParent)
      Files.write(path, lines.mkString("\n").getBytes(StandardCharsets.UTF_8))
      ()
    }

  override def spec: Spec[TestEnvironment with Scope, Any] =
    suite("ConfigFileResolverSpec")(
      test("resolve from home + parent chain, with CLI args taking highest priority") {
        val commandName = "dotcfg_test_app"

        for {
          base   <- ZIO.attempt(Files.createTempDirectory("zio-cli-config-test"))
          home   <- ZIO.attempt(Files.createDirectory(base.resolve("home")))
          root   <- ZIO.attempt(Files.createDirectory(base.resolve("workspace")))
          parent <- ZIO.attempt(Files.createDirectory(root.resolve("parent")))
          cwd    <- ZIO.attempt(Files.createDirectory(parent.resolve("cwd")))

          _ <- writeFile(home.resolve(s".$commandName"), List("--line-ending=LF", "--color=blue"))
          _ <- writeFile(root.resolve(s".$commandName"), List("--line-ending=CRLF"))
          _ <- writeFile(parent.resolve(s".$commandName"), List("--verbosity=debug"))
          _ <- writeFile(cwd.resolve(s".$commandName"), List("--line-ending=POSIX"))

          oldHome <- ZIO.attempt(Option(System.getProperty("user.home")))
          oldDir  <- ZIO.attempt(Option(System.getProperty("user.dir")))

          result <- (
                      for {
                        _       <- ZIO.attempt(System.setProperty("user.home", home.toString))
                        _       <- ZIO.attempt(System.setProperty("user.dir", cwd.toString))
                        parsed  <- ConfigFileResolver.resolveAndParse(commandName)
                        merged   = ConfigMerger.merge(parsed, List("--line-ending", "CLI"))
                      } yield (parsed, merged)
                    ).ensuring(
                      ZIO.attempt {
                        oldHome match {
                          case Some(value) => System.setProperty("user.home", value)
                          case None        => System.clearProperty("user.home")
                        }
                        oldDir match {
                          case Some(value) => System.setProperty("user.dir", value)
                          case None        => System.clearProperty("user.dir")
                        }
                      }.orDie
                    )

          (parsed, merged) = result
        } yield assert(parsed.map(_.key).toSet)(equalTo(Set("--line-ending", "--color", "--verbosity"))) &&
          assert(merged.toSet)(equalTo(Set("--color=blue", "--verbosity=debug", "--line-ending", "CLI")))
      },
      test("CliApp consumes config values when option is not passed on CLI") {
        val appName = "configdriven"

        val command = Command("configdriven", Options.text("line-ending"))

        val app = CliApp.make(
          name = appName,
          version = "0.0.1",
          summary = HelpDoc.Span.text("config test"),
          command = command
        )(lineEnding => ZIO.succeed(lineEnding))

        for {
          base <- ZIO.attempt(Files.createTempDirectory("zio-cli-config-app"))
          _    <- writeFile(base.resolve(s".$appName"), List("--line-ending=LF"))

          oldHome <- ZIO.attempt(Option(System.getProperty("user.home")))
          oldDir  <- ZIO.attempt(Option(System.getProperty("user.dir")))

          result <- (
                      for {
                        _      <- ZIO.attempt(System.setProperty("user.home", base.toString))
                        _      <- ZIO.attempt(System.setProperty("user.dir", base.toString))
                        output <- app.run(Nil)
                      } yield output
                    ).ensuring(
                      ZIO.attempt {
                        oldHome match {
                          case Some(value) => System.setProperty("user.home", value)
                          case None        => System.clearProperty("user.home")
                        }
                        oldDir match {
                          case Some(value) => System.setProperty("user.dir", value)
                          case None        => System.clearProperty("user.dir")
                        }
                      }.orDie
                    )
        } yield assert(result)(isSome(equalTo("LF")))
      }
    ) @@ TestAspect.sequential
}
