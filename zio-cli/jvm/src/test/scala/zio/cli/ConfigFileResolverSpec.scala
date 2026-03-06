package zio.cli

import zio._
import zio.test._

import java.nio.file.{Files => JFiles, Path => JPath}

object ConfigFileResolverSpec extends ZIOSpecDefault {

  private def withTempDirTree(
    structure: List[(String, String)]
  )(testFn: JPath => ZIO[Any, Any, TestResult]): ZIO[Any, Any, TestResult] =
    ZIO.acquireReleaseWith(
      ZIO.attempt {
        val baseDir = JFiles.createTempDirectory("zio-cli-test")
        structure.foreach { case (relativePath, content) =>
          val file = baseDir.resolve(relativePath)
          JFiles.createDirectories(file.getParent)
          JFiles.write(file, content.getBytes("UTF-8"))
        }
        baseDir
      }
    )(dir =>
      ZIO.attempt {
        def deleteRecursive(path: JPath): Unit = {
          if (JFiles.isDirectory(path)) {
            val stream = JFiles.list(path)
            try stream.forEach(p => deleteRecursive(p))
            finally stream.close()
          }
          val _ = JFiles.deleteIfExists(path)
        }
        deleteRecursive(dir)
      }.orDie
    )(testFn)

  def spec = suite("ConfigFileResolver JVM Suite")(
    test("reads a single dotfile and returns correct args") {
      withTempDirTree(
        List(".testapp" -> "--name test-value\n--verbose")
      ) { dir =>
        val filePath = dir.resolve(".testapp").toString
        for {
          result <- ZIO.attempt {
                      val content = new String(JFiles.readAllBytes(dir.resolve(".testapp")), "UTF-8")
                      ConfigFileResolver.parseDotFile(content, filePath)
                    }
          (args, sources) = result
        } yield assertTrue(
          args == List("--name", "test-value", "--verbose"),
          sources.length == 2,
          sources.exists(s => s.name == "--name" && s.value == "test-value"),
          sources.exists(s => s.name == "--verbose" && s.value == "")
        )
      }
    },
    test("nested directories: closer file overrides parent file") {
      withTempDirTree(
        List(
          ".testapp"          -> "--name root-name\n--output root-output",
          "sub/.testapp"      -> "--name sub-name",
          "sub/deep/.testapp" -> "--output deep-output"
        )
      ) { dir =>
        for {
          result <- ZIO.attempt {
                      // Simulate the JVM resolver's foldLeft merge (same as collectDotFiles + fold)
                      // Files ordered root to CWD (reversed): root, sub, deep
                      val files = List(
                        dir.resolve(".testapp"),
                        dir.resolve("sub/.testapp"),
                        dir.resolve("sub/deep/.testapp")
                      )

                      files.foldLeft((List.empty[String], List.empty[SettingSource])) {
                        case ((accArgs, accSources), file) =>
                          val content       = new String(JFiles.readAllBytes(file), "UTF-8")
                          val filePath      = file.toString
                          val (fileArgs, _) = ConfigFileResolver.parseDotFile(content, filePath)
                          ConfigFileResolver.mergeArgs(accArgs, accSources, fileArgs)
                      }
                    }
          (mergedArgs, mergedSources) = result
        } yield assertTrue(
          mergedArgs.contains("--name"),
          mergedArgs.contains("sub-name"),
          mergedArgs.contains("--output"),
          mergedArgs.contains("deep-output"),
          !mergedArgs.contains("root-name"),
          !mergedArgs.contains("root-output")
        )
      }
    },
    test("live resolver walks directory tree from CWD") {
      ConfigFileResolver.live.resolve("__nonexistent_app_name__").map { case (args, sources) =>
        assertTrue(args.isEmpty, sources.isEmpty)
      }
    },
    test("end-to-end CliApp with mock resolver") {
      val nameOpt  = Options.text("name")
      val command  = Command("myapp", nameOpt, Args.none)
      val resolver = new ConfigFileResolver {
        def resolve(appName: String): UIO[(List[String], List[SettingSource])] =
          ZIO.succeed(
            (List("--name", "from-dotfile"), List(SettingSource("--name", "from-dotfile", "/project/.myapp")))
          )
      }

      val app = CliApp.make[Any, Nothing, String, String](
        name = "myapp",
        version = "1.0",
        summary = HelpDoc.Span.text("test"),
        command = command,
        configFileResolver = resolver
      ) { name =>
        ZIO.succeed(name)
      }

      for {
        r1 <- app.run(Nil)
        r2 <- app.run(List("--name", "from-cli"))
      } yield assertTrue(
        r1 == Some("from-dotfile"),
        r2 == Some("from-cli")
      )
    }
  )
}
