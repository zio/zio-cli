package zio.cli

import java.io._
import java.nio.file.Paths

import zio.cli.HelpDoc.Paragraph
import zio.cli.HelpDoc.Span.Text
import zio.test.Assertion._
import zio.test._

object FileArgsSpec extends DefaultRunnableSpec {
  def spec = suite("ArgsSpec")(
    testM("Existing file") {
      val arg = Args.file("files", Exists.Yes).repeat

      assertM(
        arg.validate(argsFile.getAbsolutePath :: Nil, ParserOptions.default)
      )(equalTo(List.empty[String] -> List(argsFilePath)))
    },
    testM("Not Found file") {
      val arg = Args.file("files", Exists.Yes).repeat
      assertM(
        arg.validate("notFound.file" :: Nil, ParserOptions.default).either
      )(isLeft(equalTo(Paragraph(Text("Path 'notFound.file' must exist.")))))
    },
    testM("Non Existing file") {
      val arg = Args.file(Exists.No).repeat

      assertM(
        arg.validate("doesNotExist.file" :: Nil, ParserOptions.default).either
      )(isRight)
    },
    testM("Path of non regular file") {
      val arg = Args.file.repeat
      assertM(
        arg.validate("notRegular.file" :: Nil, ParserOptions.default).either
      )(isLeft(equalTo(Paragraph(Text("Expected path 'notRegular.file' to be a regular file.")))))
    },
    testM("Combination of existing and non existing files") {
      val arg = Args.file.repeat
      assertM(
        arg.validate(List(argsFile.getAbsolutePath, "nonExisting.file"), ParserOptions.default).either
      )(isLeft(equalTo(Paragraph(Text("Expected path 'nonExisting.file' to be a regular file.")))))
    },
    testM("Combination of existing files") {
      val arg = Args.file.repeat
      assertM(
        arg.validate(List(argsFile.getAbsolutePath, argsFile.getAbsolutePath), ParserOptions.default)
      )(equalTo(List.empty[String] -> List(argsFilePath, argsFilePath)))
    }
  )

  val argsFile     = new File("zio-cli/shared/src/test/scala/zio/cli/ArgsFileExample")
  val argsFilePath = Paths.get(argsFile.getAbsolutePath)

}
