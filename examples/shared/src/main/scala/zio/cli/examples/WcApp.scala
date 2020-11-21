package zio.cli.examples

import java.nio.file.Path

import zio.blocking.Blocking
import zio.cli.HelpDoc.Span.text
import zio.cli._
import zio.console.{ putStrLn, Console }
import zio.stream.ZStream.DefaultChunkSize
import zio.stream.{ ZStream, ZTransducer }
import zio.{ App, URIO, ZIO }

object WcApp extends App {

  val bytesFlag: Options[Boolean] = Options.bool("c", true)
  val linesFlag: Options[Boolean] = Options.bool("l", true)
  val wordsFlag: Options[Boolean] = Options.bool("w", true)
  val charFlag: Options[Boolean]  = Options.bool("m", false)

  case class WcOptions(bytes: Boolean, lines: Boolean, words: Boolean, char: Boolean)
  case class WcResult(
    fileName: String,
    countBytes: Option[Long],
    countLines: Option[Long],
    countWords: Option[Long],
    countChar: Option[Long]
  )

  val options = (bytesFlag :: linesFlag :: wordsFlag :: charFlag).as(WcOptions)

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
        def opt(option: Option[Long]): String = option.map(l => f"${l}%9d").getOrElse("")
        s"${opt(res.countLines)} ${opt(res.countWords)} ${opt(res.countChar)} ${opt(res.countBytes)} ${res.fileName}"
      }
      ZIO.foreach(res)(r => putStrLn(format(r))) *> ZIO.when(res.length > 1)(putStrLn(format(wcTotal(res)))).ignore
    }

    (opts, paths) => {
      zio.console.putStrLn(s"executing wc with args: ${opts} ${paths}") *>
        ZIO
          .foreachParN[Blocking, Throwable, Path, WcResult, List](4)(paths) { path =>
            ZStream.fromFile(path).broadcastDynamic(DefaultChunkSize).use { bytesFanOut =>
              val utf8FanOut = bytesFanOut.map(_.transduce(ZTransducer.utfDecode))
              val chars      = utf8FanOut.map(_.map(_.length))
              val words      = utf8FanOut.map(_.transduce(ZTransducer.splitOn(" ")))
              val lines      = utf8FanOut.map(_.transduce(ZTransducer.splitLines))

              def runCountOrNone(optStream: Option[ZStream[Any, Throwable, _]]) =
                optStream.map(_.runCount.orDie.optional).getOrElse(ZIO.none)
              def runSumOrNone[A](optStream: Option[ZStream[Any, Throwable, Int]]): ZIO[Any, Throwable, Option[Long]] =
                optStream.map(_.runSum.orDie.optional).getOrElse(ZIO.none).map(_.map(_.asInstanceOf[Long]))

              for {
                bytesStream <- if (opts.bytes) bytesFanOut.optional else ZIO.none
                charsStream <- if (opts.char) chars.optional else ZIO.none
                wordsStream <- if (opts.words) words.optional else ZIO.none
                linesStream <- if (opts.lines) lines.optional else ZIO.none

                bytesFiber <- runCountOrNone(bytesStream).fork
                charsFiber <- runSumOrNone(charsStream).fork
                wordsFiber <- runCountOrNone(wordsStream).fork
                linesFiber <- runCountOrNone(linesStream).fork

                bytes <- bytesFiber.join
                chars <- charsFiber.join
                words <- wordsFiber.join
                lines <- linesFiber.join
              } yield WcResult(path.getFileName.toString, bytes, lines, words, chars)
            }
          }
          .orDie
          .provideLayer(zio.blocking.Blocking.live)
          .flatMap(res => printResult(res))
    }
  }

  val wcApp = CLIApp(
    "ZIO Word Count",
    "0.1.2",
    text("counts words in the file"),
    wc,
    execute.tupled
  )

  @Override
  def run(args: List[String]) = wcApp.run(args)
}
