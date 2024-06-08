package zio.cli

import zio._
import zio.internal.stacktracer.SourceLocation
import zio.test._

import java.nio.file.{Files, Path}

object LiveFileArgsSpec extends ZIOSpecDefault {

  private val createTempDirectory: RIO[Scope, Path] =
    for {
      random <- Random.nextUUID
      path <-
        ZIO.attempt(Files.createTempDirectory(random.toString)).withFinalizer(f => ZIO.attempt(f.toFile.delete()).orDie)
    } yield path

  private def resolvePath(path: Path, paths: List[String]): Path =
    if (paths.nonEmpty) path.resolve(paths.mkString("/"))
    else path

  private def makeTest(name: String)(cwd: List[String], home: List[String])(
    writeFiles: (List[String], String)*
  )(
    exp: (List[String], List[String])*
  )(implicit loc: SourceLocation): Spec[Scope, Throwable] =
    test(name) {
      for {
        // setup
        dir <- createTempDirectory
        _   <- TestSystem.putProperty("user.dir", resolvePath(dir, cwd).toString)
        _   <- TestSystem.putProperty("user.home", resolvePath(dir, home).toString)
        _ <- ZIO.foreachDiscard(writeFiles) { case (paths, contents) =>
               val writePath  = resolvePath(dir, paths :+ s".$cmd")
               val parentFile = writePath.getParent.toFile
               ZIO.attempt(parentFile.mkdirs()).unlessZIO(ZIO.attempt(parentFile.exists())) *>
                 ZIO.writeFile(writePath, contents)
             }

        // test
        result <- FileArgs.Live.getArgsFromFile(cmd)
        resolvedExp = exp.toList.map { case (paths, args) =>
                        FileArgs.ArgsFromFile(resolvePath(dir, paths :+ s".$cmd").toString, args)
                      }

      } yield assertTrue(result == resolvedExp)
    }

  private val cmd: String = "command"

  override def spec: Spec[TestEnvironment with Scope, Any] =
    suite("FileArgsSpec")(
      makeTest("empty")(List("abc", "home"), List("abc", "home"))()(),
      makeTest("home in cwd parent path")(List("abc", "home", "d", "e", "f"), List("abc", "home"))(
        List("abc", "home", "d")           -> "d\nd\n\n",
        List("abc", "home", "d", "e")      -> "e\ne\n\n",
        List("abc", "home", "d", "e", "f") -> "f\nf\n\n",
        List("abc", "home")                -> "_home_"
      )(
        List("abc", "home", "d", "e", "f") -> List("f", "f"),
        List("abc", "home", "d", "e")      -> List("e", "e"),
        List("abc", "home", "d")           -> List("d", "d"),
        List("abc", "home")                -> List("_home_") // only appears once
      ),
      makeTest("home not in cwd parent path")(List("abc", "cwd", "d", "e", "f"), List("abc", "home"))(
        List("abc", "cwd", "d")           -> "d\nd\n\n",
        List("abc", "cwd", "d", "e")      -> "e\ne\n\n",
        List("abc", "cwd", "d", "e", "f") -> "f\nf\n\n",
        List("abc", "home")               -> "_home_"
      )(
        List("abc", "cwd", "d", "e", "f") -> List("f", "f"),
        List("abc", "cwd", "d", "e")      -> List("e", "e"),
        List("abc", "cwd", "d")           -> List("d", "d"),
        List("abc", "home")               -> List("_home_")
      ),
      makeTest("parent dirs of home are not searched")(Nil, List("abc", "home"))(
        List("abc") -> "a\nb"
      )(
      )
    ) @@ TestAspect.withLiveRandom

}
