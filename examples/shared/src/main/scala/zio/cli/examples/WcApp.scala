package zio.cli.examples

import java.nio.file.Path

import zio.blocking.Blocking
import zio.cli.HelpDoc.Span.text
import zio.cli._
import zio.console.{putStrLn, Console}
import zio.stream.{ZSink, ZStream, ZTransducer}
import zio.{App, URIO, ZIO}

object WcApp extends App {

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

  val options = (bytesFlag ++ linesFlag ++ wordsFlag ++ charFlag).as(WcOptions)

  val args = Args.file("files", Exists.Yes).repeat1

  val wc = Command("wc", options, args)

  val execute: (WcOptions, ::[Path]) => URIO[Console, Unit] = {
    def printResult(res: List[WcResult]): ZIO[Console, Nothing, Unit] = {
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

      ZIO.foreach_(res)(r => putStrLn(format(r)).!) *> ZIO.when(res.length > 1)(putStrLn(format(wcTotal(res))).!).ignore
    }

    (opts, paths) => {
      zio.console.putStrLn(s"executing wc with args: $opts $paths").! *>
        ZIO
          .foreachParN[Blocking, Throwable, Path, WcResult, List](4)(paths) { path =>
            def option(bool: Boolean, sink: ZSink[Any, Nothing, Byte, Byte, Long])
              : ZSink[Any, Nothing, Byte, Byte, Option[Long]] =
              if (bool) sink.map(Some(_)) else ZSink.succeed[Byte, Option[Long]](None)

            val byteCount = option(opts.bytes, ZSink.count)
            val lineCount = option(opts.lines, ZTransducer.utfDecode >>> ZTransducer.splitLines >>> ZSink.count)
            val wordCount = option(opts.words, ZTransducer.utfDecode >>> ZTransducer.splitOn(" ") >>> ZSink.count)
            val charCount =
              option(opts.char, ZTransducer.utfDecode >>> ZSink.foldLeft[String, Long](0L)((s, e) => s + e.length))

            val zippedSinks: ZSink[Any, Nothing, Byte, Byte, (Option[Long], Option[Long], Option[Long], Option[Long])] =
              (byteCount <&> lineCount <&> wordCount <&> charCount).map(t => (t._1._1._1, t._1._1._2, t._1._2, t._2))

            ZStream
              .fromFile(path)
              .run(zippedSinks)
              .map(t => WcResult(path.getFileName.toString, t._1, t._2, t._3, t._4))
          }
          .orDie
          .provideLayer(zio.blocking.Blocking.live)
          .flatMap(res => printResult(res))
    }
  }

  val wcApp = CliApp(
    "ZIO Word Count",
    "0.1.2",
    text("counts words in the file"),
    wc,
    execute.tupled
  )

  override def run(args: List[String]) = wcApp.run(args)
}
