package zio.cli

import zio._
import zio.internal.stacktracer.SourceLocation
import zio.test.Assertion._
import zio.test._

object FileOptionsOverrideSpec extends ZIOSpecDefault {

  // Constant FileOptions impl that returns whatever priority-ordered list the test sets up. We avoid
  // touching the real filesystem here on purpose — those concerns are exercised by `LiveFileOptionsSpec`.
  private final case class Const(fileArgs: List[FileOptions.OptionsFromFile]) extends FileOptions {
    override def getOptionsFromFiles(command: String): UIO[List[FileOptions.OptionsFromFile]] = ZIO.succeed(fileArgs)
  }

  private final case class Result(
    arg1: String,
    arg2: String,
    arg3: String,
    arg4: String
  )

  private val options: Options[Result] =
    (
      Options.text("arg-1") ++
        Options.text("arg-2") ++
        Options.text("arg-3") ++
        Options.text("arg-4")
    ).map((Result.apply _).tupled)

  private val cliApp: CliApp[Any, Nothing, Result] =
    CliApp.make(
      "test-app",
      "v0",
      HelpDoc.Span.empty,
      Command("cmd", options)
    )(ZIO.succeed(_))

  private def makeTest(
    name: String
  )(cmdLine: String*)(fromFiles: (String, List[String])*)(exp: Result)(implicit loc: SourceLocation): Spec[Any, Any] =
    test(name) {
      cliApp
        .runWithFileArgs(cmdLine.toList)
        .map(assert(_)(isSome(equalTo(exp))))
        .provideLayer(ZLayer.succeed[FileOptions](Const(fromFiles.toList.map { case (p, a) =>
          FileOptions.OptionsFromFile(p, a)
        })))
    }

  // Smoke test that runWithoutFileArgs really pins Noop and ignores any FileOptions in scope.
  private def disabledFileOptionsTest: Spec[Any, Any] =
    test("runWithoutFileArgs ignores any FileOptions present in the environment") {
      cliApp
        .runWithoutFileArgs(List("--arg-1=cli", "--arg-2=cli", "--arg-3=cli", "--arg-4=cli"))
        .map(assert(_)(isSome(equalTo(Result("cli", "cli", "cli", "cli")))))
    }

  // Smoke test for the unique-top-level-name guard: an OrElse root must not crash; it just skips file lookup.
  private def orElseRootTest: Spec[Any, Any] = {
    val left  = Command("alpha", Options.text("flag")).map(s => ("alpha", s))
    val right = Command("beta", Options.text("flag")).map(s => ("beta", s))
    val app =
      CliApp.make("multi", "v0", HelpDoc.Span.empty, left.orElse(right))(ZIO.succeed(_))
    test("OrElse root: file-options pass is skipped, parsing still succeeds") {
      app
        .runWithFileArgs(List("alpha", "--flag", "ok"))
        .map(assert(_)(isSome(equalTo(("alpha", "ok")))))
        .provideLayer(ZLayer.succeed[FileOptions](Const(List(FileOptions.OptionsFromFile("/x/.alpha", List("ignored"))))))
    }
  }

  override def spec: Spec[TestEnvironment with Scope, Any] =
    suite("FileOptionsOverrideSpec")(
      suite("option+param together")(
        makeTest("all file args overridden")("--arg-1=arg", "--arg-2=arg", "--arg-3=arg", "--arg-4=arg")(
          "/a/b/c/.cmd" -> List("--arg-1=/a/b/c.cmd"),
          "/a/b/.cmd"   -> List("--arg-2=/a/b.cmd"),
          "/a/.cmd"     -> List("--arg-3=/a.cmd")
        )(Result("arg", "arg", "arg", "arg")),
        makeTest("inheritance hierarchy")("--arg-1=arg")(
          "/a/b/c/.cmd" -> List("--arg-1=/a/b/c.cmd", "--arg-2=/a/b/c.cmd"),
          "/a/b/.cmd"   -> List("--arg-1=/a/b.cmd", "--arg-2=/a/b.cmd", "--arg-3=/a/b.cmd"),
          "/a/.cmd"     -> List("--arg-1=/a.cmd", "--arg-2=/a.cmd", "--arg-3=/a.cmd", "--arg-4=/a.cmd")
        )(Result("arg", "/a/b/c.cmd", "/a/b.cmd", "/a.cmd"))
      ),
      suite("option+param separate")(
        makeTest("all file args overridden")("--arg-1", "arg", "--arg-2", "arg", "--arg-3", "arg", "--arg-4", "arg")(
          "/a/b/c/.cmd" -> List("--arg-1", "/a/b/c.cmd"),
          "/a/b/.cmd"   -> List("--arg-2", "/a/b.cmd"),
          "/a/.cmd"     -> List("--arg-3", "/a.cmd")
        )(Result("arg", "arg", "arg", "arg")),
        makeTest("inheritance hierarchy")("--arg-1", "arg")(
          "/a/b/c/.cmd" -> List("--arg-1", "/a/b/c.cmd", "--arg-2", "/a/b/c.cmd"),
          "/a/b/.cmd"   -> List("--arg-1", "/a/b.cmd", "--arg-2", "/a/b.cmd", "--arg-3", "/a/b.cmd"),
          "/a/.cmd"     -> List("--arg-1", "/a.cmd", "--arg-2", "/a.cmd", "--arg-3", "/a.cmd", "--arg-4", "/a.cmd")
        )(Result("arg", "/a/b/c.cmd", "/a/b.cmd", "/a.cmd"))
      ),
      disabledFileOptionsTest,
      orElseRootTest
    )

}
