package zio.cli

import zio._
import zio.test.Assertion._
import zio.test._

object CliAppSpec extends ZIOSpecDefault {

  private val exitCodeSpec = {
    sealed trait Subcommand extends Product with Serializable
    object Subcommand {
      final case class Make(int: BigInt) extends Subcommand
    }

    val makeCmd: Command[Subcommand] =
      Command(
        name = "make",
        args = Args.integer
      ).map(v => Subcommand.Make(v))

    val exampleCmd: Command[Subcommand] = Command("example").subcommands(makeCmd)

    def app(behavior: Task[Unit] = ZIO.unit): CliApp[Any, Throwable, Unit] = CliApp.make(
      name = "Grumpy",
      version = "0.0.1",
      summary = HelpDoc.Span.text("this is some doc"),
      command = exampleCmd
    ) { case Subcommand.Make(_) => behavior }

    suite("exit code of the CLI")(
      test("should exit with 0 when the command is successful") {
        val result = app().run(List("make", "1")).exit

        assertZIO(result)(succeeds(anything))
      },
      test("should exit with a code <> 0 when the parsing of the command fails") {
        val result = app().run(List("make", "this is not an integer")).exit

        assertZIO(result)(fails(anything))
      },
      test("should exit with a code <> 0 when the command fails") {
        val result = app(behavior = ZIO.fail(new RuntimeException("Boom"))).run(List("make", "1")).exit

        assertZIO(result)(fails(anything))
      }
    )
  }

  override def spec: Spec[TestEnvironment & Scope, Any] =
    suite("CliApp")(
      exitCodeSpec,
      fileOptionsSpec
    )

  private def testFileSystem(files: Map[String, String]): zio.cli.files.FileSystem = {
    def normalize(p: String): String = {
      val segments = p.split('/').foldLeft(List.empty[String]) {
        case (acc, "" | ".") => acc
        case (acc, "..")     => if (acc.nonEmpty) acc.dropRight(1) else acc
        case (acc, seg)      => acc :+ seg
      }
      if (segments.isEmpty) "/" else "/" + segments.mkString("/")
    }

    val normFiles: Map[String, String] = files.map { case (k, v) => normalize(k) -> v }

    def parentDirsOf(p: String, acc: Set[String]): Set[String] = {
      val norm = normalize(p)
      val idx  = norm.lastIndexOf('/')
      if (idx > 0) {
        val parent = norm.substring(0, idx)
        parentDirsOf(parent, acc + parent)
      } else if (idx == 0) {
        acc + "/"
      } else acc
    }

    val normDirs: Set[String] =
      normFiles.keys.foldLeft(Set("/")) { (acc, k) =>
        parentDirsOf(k, acc)
      }

    new zio.cli.files.FileSystem {
      override def parsePath(path: String): IO[String, JPath] =
        zio.cli.files.FileSystem.live.parsePath(normalize(path))

      override def exists(path: JPath): UIO[Boolean] = {
        val key = normalize(pathString(path))
        ZIO.succeed(normFiles.contains(key) || normDirs.contains(key))
      }

      override def isDirectory(path: JPath): UIO[Boolean] =
        ZIO.succeed(normDirs.contains(normalize(pathString(path))))

      override def isRegularFile(path: JPath): UIO[Boolean] =
        ZIO.succeed(normFiles.contains(normalize(pathString(path))))

      override def getParent(path: JPath): UIO[Option[JPath]] = {
        val norm = normalize(pathString(path))
        if (norm.isEmpty || norm == "/") ZIO.none
        else {
          val idx    = norm.lastIndexOf('/')
          val parent = if (idx <= 0) "/" else norm.substring(0, idx)
          zio.cli.files.FileSystem.live.parsePath(parent).option
        }
      }

      override def resolve(parent: JPath, child: String): IO[String, JPath] = {
        val normParent = normalize(pathString(parent))
        val resolved   = if (normParent == "/") s"/$child" else s"$normParent/$child"
        zio.cli.files.FileSystem.live.parsePath(resolved)
      }

      override def readFile(path: JPath): IO[String, String] =
        normFiles.get(normalize(pathString(path))) match {
          case Some(content) => ZIO.succeed(content)
          case None          => ZIO.fail(s"Cannot read file '$path'.")
        }
    }
  }

  private val fileOptionsSpec =
    suite("file options (.command dotfiles)")(
      test("merges options from user.home, parent directories, and CWD in priority order and prints diagnostics") {
        val options =
          Options.text("opt1") ++ Options.text("opt2") ++ Options.text("opt3") ++ Options.text("opt4")
        val cmd = Command("mycli", options, Args.text)
        val app = CliApp.make(
          name = "mycli",
          version = "0.1.0",
          summary = HelpDoc.Span.text("test cli"),
          command = cmd
        )(res => ZIO.succeed(res))

        val baseDir = "/test-root"
        val homeDir = s"$baseDir/home"
        val parent1 = s"$baseDir/workspace"
        val parent2 = s"$parent1/sub"
        val cwdDir  = s"$parent2/project"
        val fs      = testFileSystem(
          Map(
            s"$homeDir/.mycli" -> "--opt1=from-home\n--opt2=from-home\n--opt3=from-home\n--opt4=\"from-home\"\n",
            s"$parent1/.mycli" -> "--opt1 from-parent1\n--opt2=from-parent1\n--opt3='from-parent1'\n",
            s"$cwdDir/.mycli"  -> "# comment line\n--opt1=from-cwd\n--opt2 from-cwd\n"
          )
        )
        val layer = ZLayer.succeed[FileOptions](FileOptions.FromFileSystem(fs))

        for {
          _      <- TestSystem.putProperty("user.home", homeDir)
          _      <- TestSystem.putProperty("user.dir", cwdDir)
          res    <- app.runWithFileArgs(List("--opt1", "from-cli", "positional-arg")).provideLayer(layer)
          stdout <- TestConsole.output
          stderr <- TestConsole.outputErr
        } yield assertTrue(
          res == Some((("from-cli", "from-cwd", "from-parent1", "from-home"), "positional-arg")),
          stdout.isEmpty,
          stderr.exists(line => line.contains("--opt2") && line.contains(s"$cwdDir/.mycli")),
          stderr.exists(line => line.contains("--opt3") && line.contains(s"$parent1/.mycli")),
          stderr.exists(line => line.contains("--opt4") && line.contains(s"$homeDir/.mycli")),
          !stderr.exists(line =>
            line.contains("from-cwd") || line.contains("from-parent1") || line.contains("from-home")
          ),
          !stderr.exists(line => line.contains("--opt1") && line.contains(".mycli"))
        )
      },
      test("home directory in CWD parent chain is deduplicated and retains lowest file priority") {
        val options = Options.text("opt1") ++ Options.text("opt2")
        val cmd     = Command("dedupcmd", options)
        val app     = CliApp.make("dedupcmd", "0.1.0", HelpDoc.Span.empty, cmd)(ZIO.succeed(_))

        val baseDir = "/test-dedup"
        val homeDir = s"$baseDir/users/alice"
        val subDir  = s"$homeDir/work"
        val cwdDir  = s"$subDir/repo"
        val fs      = testFileSystem(
          Map(
            s"$baseDir/users/.dedupcmd" -> "--opt1=from-users-parent\n--opt2=\"\"\n",
            s"$homeDir/.dedupcmd"       -> "--opt1=from-home\n--opt2=from-home\n",
            s"$subDir/.dedupcmd"        -> "--opt1=from-sub\n"
          )
        )
        val fileOptions = FileOptions.FromFileSystem(fs)

        for {
          _     <- TestSystem.putProperty("user.home", s"$homeDir/.")
          _     <- TestSystem.putProperty("user.dir", cwdDir)
          files <- fileOptions.getOptionsFromFiles("dedupcmd")
          res   <- app.runWithFileArgs(Nil).provideLayer(ZLayer.succeed[FileOptions](fileOptions))
        } yield assertTrue(
          files.map(_.path).distinct.size == files.size,
          res == Some(("from-sub", ""))
        )
      },
      test("supports OrElse top-level commands, Subcommands, aliases, and Args-only commands") {
        val leftCmd   = Command("cmdLeft", Options.text("opt").alias("o"), Args.integer("num"))
        val rightCmd  = Command("cmdRight", Options.text("opt"), Args.integer("num"))
        val orElseApp = CliApp.make("orElseApp", "0.1.0", HelpDoc.Span.empty, leftCmd | rightCmd)(ZIO.succeed(_))

        val argsOnlyApp =
          CliApp.make("argsOnlyApp", "0.1.0", HelpDoc.Span.empty, Command("argsOnly", Args.integer("num")))(
            ZIO.succeed(_)
          )

        val subCmd =
          Command("gitcmd", Options.boolean("verbose").alias("v")).subcommands(
            Command("clone", Options.integer("depth"), Args.text("repo"))
          )
        val subApp = CliApp.make("gitcmd", "0.1.0", HelpDoc.Span.empty, subCmd)(ZIO.succeed(_))

        val baseDir = "/test-complex"
        val cwdDir  = s"$baseDir/cwd"
        val home    = s"$baseDir/home"
        val fs      = testFileSystem(
          Map(
            s"$cwdDir/.cmdLeft"  -> "-o=fileLeft\n",
            s"$cwdDir/.argsOnly" -> "--opt=ignored\n",
            s"$home/.gitcmd"     -> "-v\n--depth=1\n",
            s"$cwdDir/.gitcmd"   -> "--depth=5\n"
          )
        )
        val layer = ZLayer.succeed[FileOptions](FileOptions.FromFileSystem(fs))

        for {
          _    <- TestSystem.putProperty("user.home", home)
          _    <- TestSystem.putProperty("user.dir", cwdDir)
          res1 <- orElseApp.runWithFileArgs(List("cmdLeft", "4")).provideLayer(layer)
          res2 <- orElseApp.runWithFileArgs(List("cmdLeft", "--opt", "cliOverride", "4")).provideLayer(layer)
          res3 <- argsOnlyApp.runWithFileArgs(List("4")).provideLayer(layer)
          res4 <- subApp.runWithFileArgs(List("clone", "https://example.com/repo.git")).provideLayer(layer)
          res5 <- subApp.runWithoutFileArgs(List("clone", "--depth", "10", "https://example.com/repo.git"))
        } yield assertTrue(
          res1 == Some(("fileLeft", BigInt(4))),
          res2 == Some(("cliOverride", BigInt(4))),
          res3 == Some(BigInt(4)),
          res4 == Some((true, (BigInt(5), "https://example.com/repo.git"))),
          res5 == Some((false, (BigInt(10), "https://example.com/repo.git")))
        )
      },
      test("FileOptions.Live discovers real files via FileSystem.live") {
        // Smoke test verifying FileSystem.live path resolution and file reading against repo root .scalafmt.conf
        // while isolating user.home via TestSystem so the host home directory is not inspected.
        for {
          liveCwd <- Live.live(zio.cli.files.FileSystem.live.currentWorkingDirectory)
          cwd      = liveCwd.getOrElse(".")
          _       <- TestSystem.putProperty("user.dir", cwd)
          _       <- TestSystem.putProperty("user.home", cwd)
          files   <- FileOptions.Live.getOptionsFromFiles("scalafmt.conf")
        } yield assertTrue(
          files.nonEmpty,
          files.head.path.endsWith(".scalafmt.conf"),
          files.head.rawArgs.nonEmpty
        )
      },
      test("--help output documents .<name> file options lookup and priority") {
        val cmd                               = Command("wc", Options.text("line-ending"))
        val app: CliApp[Any, Nothing, String] = CliApp.make(
          name = "wc",
          version = "0.1.0",
          summary = HelpDoc.Span.text("word count"),
          command = cmd
        )(ZIO.succeed(_))

        for {
          _      <- app.run(List("--help"))
          output <- TestConsole.output
          joined  = output.mkString
        } yield assertTrue(
          joined.contains(".wc"),
          joined.toLowerCase.contains("current working directory"),
          joined.toLowerCase.contains("parent"),
          joined.toLowerCase.contains("home")
        )
      }
    )
}
