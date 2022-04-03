package zio.cli.completion

import zio.{Scope, ZTraceElement, ZIO}
import zio.stream.{ZStream, ZSink}
import zio.test.Assertion._
import zio.test._
import zio.cli._
import java.io.IOException
import java.nio.file.{Files => JFiles, Path => JPath}

import scala.language.postfixOps

object CompletionSpec extends ZIOSpecDefault {

  /**
   * A micro potpourri of functions borrowed (in simplified form) from `zio-nio`.
   * TODO: Use `zio-nio` as a dependency when it has been upgraded to use
   * zio >= 2.0.0-RC3.
   */
  object Files {
    def createTempDirectoryScoped()(implicit trace: ZTraceElement): ZIO[Scope, IOException, JPath] =
      ZIO.acquireRelease(createTempDirectory())(release = deleteRecursive(_).ignore)

    def createTempDirectory()(implicit trace: ZTraceElement): ZIO[Any, IOException, JPath] =
      ZIO
        .attemptBlocking(JFiles.createTempDirectory(null))
        .refineToOrDie[IOException]

    def deleteRecursive(path: JPath)(implicit trace: ZTraceElement): ZIO[Any, IOException, Long] =
      newDirectoryStream(path).mapZIO(delete).run(ZSink.count) <* delete(path)

    def newDirectoryStream(
      dir: JPath
    )(implicit trace: ZTraceElement): ZStream[Any, IOException, JPath] =
      ZStream
        .fromJavaIteratorScoped[Any, JPath](
          ZIO
            .fromAutoCloseable(
              ZIO.attemptBlocking(JFiles.newDirectoryStream(dir))
            )
            .map(_.iterator())
        )
        .refineToOrDie[IOException]

    def delete(path: JPath)(implicit trace: ZTraceElement): ZIO[Any, IOException, Unit] =
      ZIO.attemptBlocking(JFiles.delete(path)).refineToOrDie[IOException]

    def createFile(path: JPath)(implicit trace: ZTraceElement): ZIO[Any, IOException, Unit] =
      ZIO.attemptBlocking(JFiles.createFile(path)).unit.refineToOrDie[IOException]

    def createDirectory(path: JPath)(implicit trace: ZTraceElement): ZIO[Any, IOException, Unit] =
      ZIO.attemptBlocking(JFiles.createDirectory(path)).unit.refineToOrDie[IOException]
  }

  implicit class JPathOps(self: JPath) {
    def /(other: String): JPath = self.resolve(other)
  }

  def spec = suite("Completion Spec")(
    suite("Toplevel Command Completion Spec")(
      suite("Command name")(
        test("A different command name in the args list should not affect completion") {
          assertM(
            Completion.complete(
              List("foo-alias"),
              1,
              Command("foo", Options.Empty, Args.enumeration(("bar", 1), ("baz", 2), ("bippy", 3))),
              CliConfig.default
            )
          )(equalTo(List("bar ", "baz ", "bippy ")))
        }
      ),
      suite("Command that accepts no Args or Options")(
        test("Returns no completions") {
          assertM(
            Completion.complete(List("true"), 1, True.command, CliConfig.default)
          )(equalTo(List.empty))
        }
      ),
      suite("Command that accepts no Options, single Args")(
        suite("PrimType.Bool")(
          test("No partial word should complete with 'false' and 'true'")(
            assertM(
              Completion
                .complete(List("foo"), 1, Command("foo", Options.Empty, Args.bool), CliConfig.default)
            )(equalTo(List("false ", "true ")))
          ),
          test("Partial word 'f' should complete with 'false'")(
            assertM(
              Completion.complete(
                List("foo", "f"),
                1,
                Command("foo", Options.Empty, Args.bool),
                CliConfig.default
              )
            )(equalTo(List("false ")))
          ),
          test("Partial word 't' should complete with 'true'")(
            assertM(
              Completion.complete(
                List("foo", "t"),
                1,
                Command("foo", Options.Empty, Args.bool),
                CliConfig.default
              )
            )(equalTo(List("true ")))
          ),
          test("Partial word 'true' should return 'true'")(
            assertM(
              Completion.complete(
                List("foo", "true"),
                1,
                Command("foo", Options.Empty, Args.bool),
                CliConfig.default
              )
            )(equalTo(List("true ")))
          ),
          test("Partial word 'false' should return 'false'")(
            assertM(
              Completion.complete(
                List("foo", "false"),
                1,
                Command("foo", Options.Empty, Args.bool),
                CliConfig.default
              )
            )(equalTo(List("false ")))
          ),
          test("Partial word 'x' should return no completions")(
            assertM(
              Completion.complete(
                List("foo", "x"),
                1,
                Command("foo", Options.Empty, Args.bool),
                CliConfig.default
              )
            )(equalTo(List.empty))
          )
        ),
        suite("PrimType.Decimal")(
          test("No partial word should return no completions")(
            assertM(
              Completion.complete(
                List("foo"),
                1,
                Command("foo", Options.Empty, Args.decimal),
                CliConfig.default
              )
            )(equalTo(List.empty))
          ),
          test("Partial word '32.6' should return no completions")(
            assertM(
              Completion.complete(
                List("foo", "32.6"),
                1,
                Command("foo", Options.Empty, Args.decimal),
                CliConfig.default
              )
            )(equalTo(List.empty))
          ),
          test("Partial word 'x' should return no completions")(
            assertM(
              Completion.complete(
                List("foo", "32.6"),
                1,
                Command("foo", Options.Empty, Args.decimal),
                CliConfig.default
              )
            )(equalTo(List.empty))
          )
        ),
        suite("PrimType.Enumeration")(
          test("No partial word should return the complete list of enumeration values")(
            assertM(
              Completion.complete(
                List("foo"),
                1,
                Command("foo", Options.Empty, Args.enumeration(("bar", 1), ("baz", 2), ("bippy", 3))),
                CliConfig.default
              )
            )(equalTo(List("bar ", "baz ", "bippy ")))
          ),
          test("Partial word 'b' should complete with 'bar', 'baz', 'bippy'")(
            assertM(
              Completion.complete(
                List("foo", "b"),
                1,
                Command("foo", Options.Empty, Args.enumeration(("bar", 1), ("baz", 2), ("bippy", 3))),
                CliConfig.default
              )
            )(equalTo(List("bar ", "baz ", "bippy ")))
          ),
          test("Partial word 'ba' should complete with 'bar' and 'baz'")(
            assertM(
              Completion.complete(
                List("foo", "ba"),
                1,
                Command("foo", Options.Empty, Args.enumeration(("bar", 1), ("baz", 2), ("bippy", 3))),
                CliConfig.default
              )
            )(equalTo(List("bar ", "baz ")))
          ),
          test("Partial word 'baz' should return 'baz")(
            assertM(
              Completion.complete(
                List("foo", "baz"),
                1,
                Command("foo", Options.Empty, Args.enumeration(("bar", 1), ("baz", 2), ("bippy", 3))),
                CliConfig.default
              )
            )(equalTo(List("baz ")))
          ),
          test("Partial word 'baz' should return 'baz' and 'bazinga'")(
            assertM(
              Completion.complete(
                List("foo", "baz"),
                1,
                Command("foo", Options.Empty, Args.enumeration(("bar", 1), ("baz", 2), ("bazinga", 3))),
                CliConfig.default
              )
            )(equalTo(List("baz ", "bazinga ")))
          )
        ),
        suite("PrimType.Path")(
          test("Args.file, no prefix provided")(
            ZIO.scoped[CompletionSpec.Environment](
              Files
                .createTempDirectoryScoped()
                .flatMap(tempDir =>
                  Files.createFile(tempDir / "foo.txt") *>
                    Files.createFile(tempDir / "bar.pdf") *>
                    Files.createFile(tempDir / "bippy.sh") *>
                    Files.createDirectory(tempDir / "fooDir") *>
                    Files.createDirectory(tempDir / "barDir") *>
                    Files.createDirectory(tempDir / "bippyDir") *>
                    assertM(
                      Completion.complete(
                        List("program"),
                        1,
                        Command("program", Options.Empty, Args.file),
                        CliConfig.default,
                        compgen = Compgen.test(
                          tempDir.toFile
                        )
                      )
                    )(equalTo(List("bar.pdf ", "barDir/", "bippy.sh ", "bippyDir/", "foo.txt ", "fooDir/")))
                )
            )
          ),
          test("Args.file, prefix provided")(
            ZIO.scoped[CompletionSpec.Environment](
              Files
                .createTempDirectoryScoped()
                .flatMap(tempDir =>
                  Files.createFile(tempDir / "foo.txt") *>
                    Files.createFile(tempDir / "bar.pdf") *>
                    Files.createFile(tempDir / "bippy.sh") *>
                    Files.createDirectory(tempDir / "fooDir") *>
                    Files.createDirectory(tempDir / "barDir") *>
                    Files.createDirectory(tempDir / "bippyDir") *>
                    assertM(
                      Completion.complete(
                        List("program", "f"),
                        1,
                        Command("program", Options.Empty, Args.file),
                        CliConfig.default,
                        compgen = Compgen.test(
                          tempDir.toFile
                        )
                      )
                    )(equalTo(List("foo.txt ", "fooDir/")))
                )
            )
          ),
          test("Args.file, complete file name provided")(
            ZIO.scoped[CompletionSpec.Environment](
              Files
                .createTempDirectoryScoped()
                .flatMap(tempDir =>
                  Files.createFile(tempDir / "foo.txt") *>
                    Files.createFile(tempDir / "bar.pdf") *>
                    Files.createFile(tempDir / "bippy.sh") *>
                    Files.createDirectory(tempDir / "fooDir") *>
                    Files.createDirectory(tempDir / "barDir") *>
                    Files.createDirectory(tempDir / "bippyDir") *>
                    assertM(
                      Completion.complete(
                        List("program", "foo.txt"),
                        1,
                        Command("program", Options.Empty, Args.file),
                        CliConfig.default,
                        compgen = Compgen.test(
                          tempDir.toFile
                        )
                      )
                    )(equalTo(List("foo.txt ")))
                )
            )
          ),
          test("Args.file, prefix of nonexistent file should yield no completions.")(
            ZIO.scoped[CompletionSpec.Environment](
              Files
                .createTempDirectoryScoped()
                .flatMap(tempDir =>
                  Files.createFile(tempDir / "foo.txt") *>
                    Files.createFile(tempDir / "bar.pdf") *>
                    Files.createFile(tempDir / "bippy.sh") *>
                    Files.createDirectory(tempDir / "fooDir") *>
                    Files.createDirectory(tempDir / "barDir") *>
                    Files.createDirectory(tempDir / "bippyDir") *>
                    assertM(
                      Completion.complete(
                        List("program", "doesnt-exist"),
                        1,
                        Command("program", Options.Empty, Args.directory),
                        CliConfig.default,
                        compgen = Compgen.test(
                          tempDir.toFile
                        )
                      )
                    )(equalTo(List()))
                )
            )
          ),
          test("Args.directory, no prefix provided")(
            ZIO.scoped[CompletionSpec.Environment](
              Files
                .createTempDirectoryScoped()
                .flatMap(tempDir =>
                  Files.createFile(tempDir / "foo.txt") *>
                    Files.createFile(tempDir / "bar.pdf") *>
                    Files.createFile(tempDir / "bippy.sh") *>
                    Files.createDirectory(tempDir / "fooDir") *>
                    Files.createDirectory(tempDir / "barDir") *>
                    Files.createDirectory(tempDir / "bippyDir") *>
                    assertM(
                      Completion.complete(
                        List("program"),
                        1,
                        Command("program", Options.Empty, Args.directory),
                        CliConfig.default,
                        compgen = Compgen.test(
                          tempDir.toFile
                        )
                      )
                    )(equalTo(List("barDir/", "bippyDir/", "fooDir/")))
                )
            )
          ),
          test("Args.directory, prefix provided")(
            ZIO.scoped[CompletionSpec.Environment](
              Files
                .createTempDirectoryScoped()
                .flatMap(tempDir =>
                  Files.createFile(tempDir / "foo.txt") *>
                    Files.createFile(tempDir / "bar.pdf") *>
                    Files.createFile(tempDir / "bippy.sh") *>
                    Files.createDirectory(tempDir / "fooDir") *>
                    Files.createDirectory(tempDir / "barDir") *>
                    Files.createDirectory(tempDir / "bippyDir") *>
                    assertM(
                      Completion.complete(
                        List("program", "f"),
                        1,
                        Command("program", Options.Empty, Args.directory),
                        CliConfig.default,
                        compgen = Compgen.test(
                          tempDir.toFile
                        )
                      )
                    )(equalTo(List("fooDir/")))
                )
            )
          ),
          test("Args.directory, complete name provided")(
            ZIO.scoped[CompletionSpec.Environment](
              Files
                .createTempDirectoryScoped()
                .flatMap(tempDir =>
                  Files.createFile(tempDir / "foo.txt") *>
                    Files.createFile(tempDir / "bar.pdf") *>
                    Files.createFile(tempDir / "bippy.sh") *>
                    Files.createDirectory(tempDir / "fooDir") *>
                    Files.createDirectory(tempDir / "barDir") *>
                    Files.createDirectory(tempDir / "bippyDir") *>
                    assertM(
                      Completion.complete(
                        List("program", "fooDir"),
                        1,
                        Command("program", Options.Empty, Args.directory),
                        CliConfig.default,
                        compgen = Compgen.test(
                          tempDir.toFile
                        )
                      )
                    )(equalTo(List("fooDir/")))
                )
            )
          )
        ),
        suite("PrimType.ZoneId")(
          test("'US/' prefix provided")(
            assertM(
              Completion.complete(
                List("program", "US/"),
                1,
                Command("program", Options.Empty, Args.zoneId),
                CliConfig.default
              )
            )(
              equalTo(
                List(
                  "US/Alaska ",
                  "US/Aleutian ",
                  "US/Arizona ",
                  "US/Central ",
                  "US/East-Indiana ",
                  "US/Eastern ",
                  "US/Hawaii ",
                  "US/Indiana-Starke ",
                  "US/Michigan ",
                  "US/Mountain ",
                  "US/Pacific ",
                  "US/Samoa "
                )
              )
            )
          )
        )
      ),
      suite("Command with no Options, multiple Args")(
        suite("PrimType.Enumeration followed by PrimType.ZoneId")(
          test("Partial word 'baz' should return 'baz' and 'bazinga'")(
            assertM(
              Completion.complete(
                List("foo", "baz"),
                1,
                Command("foo", Options.Empty, Args.enumeration(("bar", 1), ("baz", 2), ("bazinga", 3)) ++ Args.zoneId),
                CliConfig.default
              )
            )(equalTo(List("baz ", "bazinga ")))
          ),
          test("Partial word 'US/' should return the US zone IDs")(
            assertM(
              Completion.complete(
                List("foo", "baz", "US/"),
                2,
                Command("foo", Options.Empty, Args.enumeration(("bar", 1), ("baz", 2), ("bazinga", 3)) ++ Args.zoneId),
                CliConfig.default
              )
            )(
              equalTo(
                List(
                  "US/Alaska ",
                  "US/Aleutian ",
                  "US/Arizona ",
                  "US/Central ",
                  "US/East-Indiana ",
                  "US/Eastern ",
                  "US/Hawaii ",
                  "US/Indiana-Starke ",
                  "US/Michigan ",
                  "US/Mountain ",
                  "US/Pacific ",
                  "US/Samoa "
                )
              )
            )
          ),
          test("Completing ['foo', 'baz', 'US/'] at position 1 should complete with 'baz' and 'bazinga'")(
            assertM(
              Completion.complete(
                List("foo", "baz", "US/"),
                1,
                Command("foo", Options.Empty, Args.enumeration(("bar", 1), ("baz", 2), ("bazinga", 3)) ++ Args.zoneId),
                CliConfig.default
              )
            )(
              equalTo(List("baz ", "bazinga "))
            )
          ),
          test("Completing ['foo', 'x', 'US/'] at position 2 should yield no completions ('x' is invalid)")(
            assertM(
              Completion.complete(
                List("foo", "x", "US/"),
                1,
                Command("foo", Options.Empty, Args.enumeration(("bar", 1), ("baz", 2), ("bazinga", 3)) ++ Args.zoneId),
                CliConfig.default
              )
            )(
              equalTo(List.empty)
            )
          )
        )
      ),
      suite("Command with Options, no args")(
        suite("Boolean Options")(
          test("No prefix should show all flags")(
            assertM(
              Completion.complete(
                List("foo"),
                1,
                Command(
                  "foo",
                  Options.boolean("a") ++ Options.boolean("b") ++ Options.boolean("c") ++ Options.boolean("d"),
                  Args.Empty
                ),
                CliConfig.default
              )
            )(
              equalTo(List("-a ", "-b ", "-c ", "-d "))
            )
          ),
          test("'-' prefix should show all flags")(
            assertM(
              Completion.complete(
                List("foo", "-"),
                1,
                Command(
                  "foo",
                  Options.boolean("a") ++ Options.boolean("b") ++ Options.boolean("c") ++ Options.boolean("d"),
                  Args.Empty
                ),
                CliConfig.default
              )
            )(
              equalTo(List("-a ", "-b ", "-c ", "-d "))
            )
          ),
          test("'-a' prefix should show flags '-b'")(
            assertM(
              Completion.complete(
                List("foo", "-a"),
                2,
                Command(
                  "foo",
                  Options.boolean("a") ++ Options.boolean("b"),
                  Args.Empty
                ),
                CliConfig.default
              )
            )(
              equalTo(List("-b "))
            )
          ),
          test("'-b' prefix should show flags '-a'")(
            assertM(
              Completion.complete(
                List("foo", "-b"),
                2,
                Command(
                  "foo",
                  Options.boolean("a") ++ Options.boolean("b"),
                  Args.Empty
                ),
                CliConfig.default
              )
            )(
              equalTo(List("-a "))
            )
          ),
          test("'-a' prefix should show flags '-b', '-c', '-d'")(
            assertM(
              Completion.complete(
                List("foo", "-a"),
                2,
                Command(
                  "foo",
                  Options.boolean("a") ++ Options.boolean("b") ++ Options.boolean("c") ++ Options.boolean("d"),
                  Args.Empty
                ),
                CliConfig.default
              )
            )(
              equalTo(List("-b ", "-c ", "-d "))
            )
          ),
          test("'-d -a' prefix should show flags '-b', '-c'")(
            assertM(
              Completion.complete(
                List("foo", "-d", "-a"),
                3,
                Command(
                  "foo",
                  Options.boolean("a") ++ Options.boolean("b") ++ Options.boolean("c") ++ Options.boolean("d"),
                  Args.Empty
                ),
                CliConfig.default
              )
            )(
              equalTo(List("-b ", "-c "))
            )
          ),
          test("'-d -c -b -' prefix should show flags '-a'")(
            assertM(
              Completion.complete(
                List("foo", "-d", "-c", "-b", "-"),
                4,
                Command(
                  "foo",
                  Options.boolean("a") ++ Options.boolean("b") ++ Options.boolean("c") ++ Options.boolean("d"),
                  Args.Empty
                ),
                CliConfig.default
              )
            )(
              equalTo(List("-a "))
            )
          ),
          test("An invalid flag should yield no completions")(
            assertM(
              Completion.complete(
                List("foo", "-x"),
                2,
                Command(
                  "foo",
                  Options.boolean("a") ++ Options.boolean("b") ++ Options.boolean("c") ++ Options.boolean("d"),
                  Args.Empty
                ),
                CliConfig.default
              )
            )(
              equalTo(List.empty)
            )
          )
        ),
        suite("Int Options")(
          test("No prefix should show all flags")(
            assertM(
              Completion.complete(
                List("foo"),
                1,
                Command(
                  "foo",
                  Options.integer("a") ++ Options.integer("b") ++ Options.integer("c") ++ Options.integer("d"),
                  Args.Empty
                ),
                CliConfig.default
              )
            )(
              equalTo(List("-a ", "-b ", "-c ", "-d "))
            )
          ),
          test("'-c' without integer value should provide no completions")(
            assertM(
              Completion.complete(
                List("foo", "-c"),
                2,
                Command(
                  "foo",
                  Options.integer("a") ++ Options.integer("b") ++ Options.integer("c") ++ Options.integer("d"),
                  Args.Empty
                ),
                CliConfig.default
              )
            )(
              equalTo(List.empty)
            )
          ),
          test("'-c' with integer value should complete with '-a', '-b', '-d'")(
            assertM(
              Completion.complete(
                List("foo", "-c", "1"),
                3,
                Command(
                  "foo",
                  Options.integer("a") ++ Options.integer("b") ++ Options.integer("c") ++ Options.integer("d"),
                  Args.Empty
                ),
                CliConfig.default
              )
            )(
              equalTo(List("-a ", "-b ", "-d "))
            )
          ),
          test("'-c' and '-b' with integer value should complete with '-a', '-d'")(
            assertM(
              Completion.complete(
                List("foo", "-c", "1", "-b", "2"),
                5,
                Command(
                  "foo",
                  Options.integer("a") ++ Options.integer("b") ++ Options.integer("c") ++ Options.integer("d"),
                  Args.Empty
                ),
                CliConfig.default
              )
            )(
              equalTo(List("-a ", "-d "))
            )
          ),
          test("'-c' with integer value and '-b' with no integer value should provide no completions")(
            assertM(
              Completion.complete(
                List("foo", "-c", "1", "-b"),
                4,
                Command(
                  "foo",
                  Options.integer("a") ++ Options.integer("b") ++ Options.integer("c") ++ Options.integer("d"),
                  Args.Empty
                ),
                CliConfig.default
              )
            )(
              equalTo(List.empty)
            )
          )
        ),
        suite("Enumeration Options")(
          test("Partial option name should complete the name of the option")(
            assertM(
              Completion.complete(
                List("foo", "--q"),
                1,
                Command("foo", Options.enumeration("quux")(("bar", 1), ("baz", 2), ("bippy", 3))),
                CliConfig.default
              )
            )(equalTo(List("--quux ")))
          ),
          test("No partial word should return the complete list of enumeration options")(
            assertM(
              Completion.complete(
                List("foo", "--quux"),
                2,
                Command("foo", Options.enumeration("quux")(("bar", 1), ("baz", 2), ("bippy", 3))),
                CliConfig.default
              )
            )(equalTo(List("bar ", "baz ", "bippy ")))
          ),
          test("Partial word 'b' should complete with 'bar', 'baz', 'bippy'")(
            assertM(
              Completion.complete(
                List("foo", "--quux", "b"),
                2,
                Command("foo", Options.enumeration("quux")(("bar", 1), ("baz", 2), ("bippy", 3))),
                CliConfig.default
              )
            )(equalTo(List("bar ", "baz ", "bippy ")))
          ),
          test("Partial word 'ba' should complete with 'bar' and 'baz'")(
            assertM(
              Completion.complete(
                List("foo", "--quux", "ba"),
                2,
                Command("foo", Options.enumeration("quux")(("bar", 1), ("baz", 2), ("bippy", 3))),
                CliConfig.default
              )
            )(equalTo(List("bar ", "baz ")))
          ),
          test("Partial word 'baz' should return 'baz'")(
            assertM(
              Completion.complete(
                List("foo", "--quux", "baz"),
                2,
                Command("foo", Options.enumeration("quux")(("bar", 1), ("baz", 2), ("bippy", 3))),
                CliConfig.default
              )
            )(equalTo(List("baz ")))
          ),
          test("Partial word 'baz' should return 'baz' and 'bazinga'")(
            assertM(
              Completion.complete(
                List("foo", "--quux", "baz"),
                2,
                Command("foo", Options.enumeration("quux")(("bar", 1), ("baz", 2), ("bazinga", 3))),
                CliConfig.default
              )
            )(equalTo(List("baz ", "bazinga ")))
          )
        )
      ),
      suite("Command with Options and Args")(
        suite("Tail")(
          test("Complete the '-n' option name")(
            assertM(
              Completion.complete(
                List("tail", "-"),
                1,
                Tail.command,
                CliConfig.default,
                compgen = Compgen.test(new java.io.File("./"))
              )
            )(equalTo(List("-n ")))
          ),
          test("Complete the file name")(
            ZIO.scoped[CompletionSpec.Environment](
              Files
                .createTempDirectoryScoped()
                .flatMap(tempDir =>
                  Files.createFile(tempDir / "foo.txt") *>
                    Files.createFile(tempDir / "bar.pdf") *>
                    Files.createFile(tempDir / "bippy.sh") *>
                    Files.createDirectory(tempDir / "fooDir") *>
                    Files.createDirectory(tempDir / "barDir") *>
                    Files.createDirectory(tempDir / "bippyDir") *>
                    assertM(
                      Completion.complete(
                        List("tail", "f"),
                        1,
                        Tail.command,
                        CliConfig.default,
                        compgen = Compgen.test(
                          tempDir.toFile
                        )
                      )
                    )(equalTo(List("foo.txt ", "fooDir/")))
                )
            )
          )
        ),
        suite("WC")(
          test("Complete the option names")(
            assertM(
              Completion.complete(
                List("wc", "-"),
                1,
                WC.command,
                CliConfig.default,
                compgen = Compgen.test(new java.io.File("./"))
              )
            )(equalTo(List("-c ", "-l ", "-m ", "-w ")))
          ),
          test("Complete the first file name")(
            ZIO.scoped[CompletionSpec.Environment](
              Files
                .createTempDirectoryScoped()
                .flatMap(tempDir =>
                  Files.createFile(tempDir / "foo.txt") *>
                    Files.createFile(tempDir / "bar.pdf") *>
                    Files.createFile(tempDir / "bippy.sh") *>
                    Files.createDirectory(tempDir / "fooDir") *>
                    Files.createDirectory(tempDir / "barDir") *>
                    Files.createDirectory(tempDir / "bippyDir") *>
                    assertM(
                      Completion.complete(
                        List("wc", "-l", "-c", "f"),
                        3,
                        WC.command,
                        CliConfig.default,
                        compgen = Compgen.test(
                          tempDir.toFile
                        )
                      )
                    )(equalTo(List("foo.txt ", "fooDir/")))
                )
            )
          ),
          test("Complete the second file name")(
            ZIO.scoped[CompletionSpec.Environment](
              Files
                .createTempDirectoryScoped()
                .flatMap(tempDir =>
                  Files.createFile(tempDir / "foo.txt") *>
                    Files.createFile(tempDir / "bar.pdf") *>
                    Files.createFile(tempDir / "bippy.sh") *>
                    Files.createDirectory(tempDir / "fooDir") *>
                    Files.createDirectory(tempDir / "barDir") *>
                    Files.createDirectory(tempDir / "bippyDir") *>
                    assertM(
                      Completion.complete(
                        List("wc", "-l", "-c", "blah.md", "f"),
                        4,
                        WC.command,
                        CliConfig.default,
                        compgen = Compgen.test(
                          tempDir.toFile
                        )
                      )
                    )(equalTo(List("foo.txt ", "fooDir/")))
                )
            )
          )
        )
      )
    )
  )

  /**
   * `True` is an example CLI command that takes no arguments.
   */
  object True {
    val command = Command("true", Options.Empty, Args.Empty)
  }

  object Tail {
    val nFlag = Options.integer("n").withDefault(BigInt(10))

    val options: Options[BigInt] = nFlag
    val args: Args[JPath]        = Args.file("file")

    val command = Command("tail", options, args)
  }

  object WC {
    val bytesFlag: Options[Boolean] = Options.boolean("c", true)
    val linesFlag: Options[Boolean] = Options.boolean("l", true)
    val wordsFlag: Options[Boolean] = Options.boolean("w", true)
    val charFlag: Options[Boolean]  = Options.boolean("m", false)

    val options = bytesFlag ++ linesFlag ++ wordsFlag ++ charFlag

    val args = Args.file("files") *

    val command = Command("wc", options, args)
  }

  object Ag {
    val afterFlag: Options[BigInt]  = Options.integer("after").alias("A")
    val beforeFlag: Options[BigInt] = Options.integer("before").alias("B")

    val options = afterFlag ++ beforeFlag

    val args = Args.text

    val command = Command("grep", options, args)
  }
}
