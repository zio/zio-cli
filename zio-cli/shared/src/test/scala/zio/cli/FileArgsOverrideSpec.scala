package zio.cli

import zio._
import zio.internal.stacktracer.SourceLocation
import zio.test._
import zio.test.Assertion._

object FileArgsOverrideSpec extends ZIOSpecDefault {

  private final case class Const(fileArgs: List[FileArgs.ArgsFromFile]) extends FileArgs {
    override def getArgsFromFile(command: String): UIO[List[FileArgs.ArgsFromFile]] = ZIO.succeed(fileArgs)
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
        .provideLayer(ZLayer.succeed(Const(fromFiles.toList.map { case (p, a) => FileArgs.ArgsFromFile(p, a) })))
    }

  override def spec: Spec[TestEnvironment with Scope, Any] =
    suite("FileArgsOverrideSpec")(
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
      )
    )

}
