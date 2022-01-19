package zio.cli

import java.io._
import java.nio.file.Paths

import zio.cli.HelpDoc.Paragraph
import zio.cli.HelpDoc.Span.Text
import zio.test.Assertion._
import zio.test._
import zio.test.ZIOSpecDefault

object FileArgsSpec extends ZIOSpecDefault {
  def spec = suite("ArgsSpec")(
    test("Existing file") {
      val arg = Args.file("files", Exists.Yes).repeat

      assertM(
        arg.validate(argsFile.getAbsolutePath :: Nil, CliConfig.default)
      )(equalTo(List.empty[String] -> List(argsFilePath)))
    },
    test("Not Found file") {
      val arg = Args.file("files", Exists.Yes).repeat
      assertM(
        arg.validate("notFound.file" :: Nil, CliConfig.default).either
      )(isLeft(equalTo(Paragraph(Text("Path 'notFound.file' must exist.")))))
    },
    test("Non Existing file") {
      val arg = Args.file(Exists.No).repeat

      assertM(
        arg.validate("doesNotExist.file" :: Nil, CliConfig.default).either
      )(isRight)
    },
    test("Combination of existing files") {
      val arg = Args.file.repeat
      assertM(
        arg.validate(List(argsFile.getAbsolutePath, argsFile.getAbsolutePath), CliConfig.default)
      )(equalTo(List.empty[String] -> List(argsFilePath, argsFilePath)))
    }
  )

  val argsFile     = new File("zio-cli/shared/src/test/scala/zio/cli/ArgsFileExample")
  val argsFilePath = Paths.get(argsFile.getAbsolutePath)

}
