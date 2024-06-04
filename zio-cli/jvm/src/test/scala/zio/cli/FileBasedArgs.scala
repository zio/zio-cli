import zio._
import zio.test._
import zio.test.Assertion._
import java.nio.file.{Files, Paths, Path}
import java.io.IOException
import zio.cli.ConfigFileArgsPlatformSpecific
import zio.cli.ConfigFilePlatformSpecific
import zio.cli.Options
import zio.cli.Args
import zio.cli.Exists
import zio.cli.Command
import zio.Console.printLine
import zio.stream.ZSink
import zio.stream.ZPipeline
import zio.stream.ZStream
import zio.cli.CliApp
import zio.cli.HelpDoc.Span.text

object FileBasedArgs extends ZIOSpecDefault {

  val configFileOps: ConfigFilePlatformSpecific = ConfigFileArgsPlatformSpecific

  def spec = suite("FileBasedArgs")(
    test("") {
      for {
        // Create Sample config files
        cwd     <- ZIO.succeed(Paths.get(java.lang.System.getProperty("user.dir")))
        homeDir <- ZIO.succeed(Paths.get(java.lang.System.getProperty("user.home")))
        command <- ZIO.succeed("wc")

        _ <- createLineCountTestFile(cwd, "sample_file")
        _ <- createSampleConfigFiles(cwd, homeDir, command, content_home = "-w", content_cwd = "-l")

        _      <- cliApp.run(args = List("sample_file")).either
        output <- TestConsole.output

        correctOutput <- ZIO.succeed("        3         4        20  sample_file\n")

        _ <- cleanupLineCountTestFile(cwd, "sample_file")
        _ <- cleanUpSampleConfigFiles(cwd: Path, homeDir: Path, command)
      } yield assert(output.last)(equalTo(correctOutput))
    },
    test("should load options from files and merge them appropriatly") {
      for {
        // Create Sample config files
        cwd     <- ZIO.succeed(Paths.get(java.lang.System.getProperty("user.dir")))
        homeDir <- ZIO.succeed(Paths.get(java.lang.System.getProperty("user.home")))
        command <- ZIO.succeed("testApp")
        _       <- createSampleConfigFiles(cwd, homeDir, command)

        // Check if the func checkAndGetOptionsFilePaths can
        configArgs <- configFileOps.loadOptionsFromConfigFiles(command)

        _ <- cleanUpSampleConfigFiles(cwd: Path, homeDir: Path, command)

      } yield assert(configArgs)(hasSameElements(List("home=true", "dir=true", "home=false")))
    },
    test("should return directory ~/home and ./ which have .testApp config file for loading the args") {
      for {
        // Create Sample config files
        cwd     <- ZIO.succeed(Paths.get(java.lang.System.getProperty("user.dir")))
        homeDir <- ZIO.succeed(Paths.get(java.lang.System.getProperty("user.home")))
        command <- ZIO.succeed("testApp1")
        _       <- createSampleConfigFiles(cwd, homeDir, command)

        // Check if the func checkAndGetOptionsFilePaths can
        paths <- configFileOps.findPathsOfCliConfigFiles(command)

        _ <- cleanUpSampleConfigFiles(cwd: Path, homeDir: Path, command)

      } yield assert(paths)(hasSameElements(List(homeDir.toString(), cwd.toString())))
    }
  )

  def createLineCountTestFile(
    cwd: Path,
    file_name: String = "sample_file",
    content: String = "asdf\nqweqwer\njdsafn"
  ): IO[IOException, Unit] =
    ZIO.attempt {
      Files.write(Paths.get(cwd.toString(), s"$file_name"), java.util.Arrays.asList(content));
      ()
    }.refineToOrDie[IOException]
  
  def cleanupLineCountTestFile(cwd: Path, file_name: String = "sample_file"): IO[IOException, Unit] =
    ZIO.attempt {
      Files.delete(Paths.get(cwd.toString(), s"$file_name"));
      ()
    }.refineToOrDie[IOException]

  def createSampleConfigFiles(
    cwd: Path,
    homeDir: Path,
    command: String = "testApp",
    content_home: String = "home=true",
    content_cwd: String = "dir=true\nhome=false"
  ): IO[IOException, Unit] =
    ZIO.attempt {
      Files.write(Paths.get(homeDir.toString(), s".$command"), java.util.Arrays.asList(content_home));
      Files.write(Paths.get(cwd.toString(), s".$command"), java.util.Arrays.asList(content_cwd));
      ()
    }.refineToOrDie[IOException]

  def cleanUpSampleConfigFiles(cwd: Path, homeDir: Path, command: String = "testApp"): IO[IOException, Unit] =
    ZIO.attempt {
      Files.delete(Paths.get(homeDir.toString(), s".$command"));
      Files.delete(Paths.get(cwd.toString(), s".$command"));
      ()
    }.refineToOrDie[IOException]

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
