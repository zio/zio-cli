package zio.cli.examples

import zio.Console.printLine
import zio._
import zio.cli.HelpDoc.Span.text
import zio.cli._
import zio.stream.{ZPipeline, ZSink, ZStream}

import java.nio.file.Path

object WcApp extends ZIOCliDefault {

  val bytesFlag: Options[Boolean] = Options.boolean("c")
  val linesFlag: Options[Boolean] = Options.boolean("l")
  val wordsFlag: Options[Boolean] = Options.boolean("w")
  val charFlag: Options[Boolean]  = Options.boolean("m", false)

  case class WcOptions(bytes: Boolean, lines: Boolean, words: Boolean, char: Boolean)
  case class WcResult(
    fileName: String,
    countBytes: Option[Long],
    countLines: Option[Long],
    countWords: Option[Long],
    countChar: Option[Long]
  )

  val options = (bytesFlag ++ linesFlag ++ wordsFlag ++ charFlag).as(WcOptions.apply _)

  val args = Args.file("files", Exists.Yes).repeat1

  val wc: Command[(WcOptions, ::[Path])] = Command("wc", options, args)

  val execute: (WcOptions, ::[Path]) => UIO[Unit] = {
    def printResult(res: List[WcResult]): UIO[Unit] = {
      def wcTotal(results: List[WcResult]) = {
        def optSum(acc: WcResult, elem: WcResult, extract: WcResult => Option[Long]): Option[Long] =
          extract(acc).flatMap(a => extract(elem).map(_ + a))

        results.fold(WcResult("total", Some(0), Some(0), Some(0), Some(0))) { (acc, wcRes) =>
          acc.copy(
            countBytes = optSum(acc, wcRes, _.countBytes),
            countChar = optSum(acc, wcRes, _.countChar),
            countWords = optSum(acc, wcRes, _.countWords),
            countLines = optSum(acc, wcRes, _.countLines)
          )
        }
      }

      def format(res: WcResult) = {
        def opt(option: Option[Long]): String = option.map(l => f"$l%9d").getOrElse("")

        s"${opt(res.countLines)} ${opt(res.countWords)} ${opt(res.countChar)} ${opt(res.countBytes)} ${res.fileName}"
      }

      ZIO.foreachDiscard(res)(r => printLine(format(r)).!) *> ZIO
        .when(res.length > 1)(printLine(format(wcTotal(res))).!)
        .ignore
    }

    (opts, paths) => {
      zio.Console.printLine(s"executing wc with args: $opts $paths").! *>
        ZIO
          .foreachPar[Any, Throwable, Path, WcResult, List](paths)({ path =>
            def option(bool: Boolean, sink: ZSink[Any, Throwable, Byte, Byte, Long])
              : ZSink[Any, Throwable, Byte, Byte, Option[Long]] =
              if (bool) sink.map(Some(_)) else ZSink.succeed(None)

            val byteCount = option(opts.bytes, ZSink.count)
            val lineCount = option(opts.lines, ZPipeline.utfDecode >>> ZPipeline.splitLines >>> ZSink.count)
            val wordCount =
              option(
                opts.words,
                ZPipeline.utfDecode >>> ZPipeline.mapChunks((_: Chunk[String]).flatMap(_.split("\\s+"))) >>> ZSink.count
              )
            val charCount =
              option(opts.char, ZPipeline.utfDecode >>> ZSink.foldLeft[String, Long](0L)((s, e) => s + e.length))

            val zippedSinks
              : ZSink[Any, Throwable, Byte, Byte, (Option[Long], Option[Long], Option[Long], Option[Long])] =
              (byteCount <&> lineCount <&> wordCount <&> charCount)

            ZStream
              .fromFile(path.toFile)
              .run(zippedSinks)
              .map(t => WcResult(path.getFileName.toString, t._1, t._2, t._3, t._4))
          })
          .withParallelism(4)
          .orDie
          .flatMap(res => printResult(res))
    }
  }

  val cliApp = CliApp.make(
    "ZIO Word Count",
    "0.1.2",
    text("counts words in the file"),
    wc
  )(execute.tupled)
}
