import zio._
import zio.test._
import zio.test.Assertion._
import java.nio.file.{Files, Paths, Path}
import zio.cli.CliApp
import java.io.IOException

object FileBasedArgs extends ZIOSpecDefault {
  def spec = suite("Vivasvan")(
    test("should load options from files and merge them appropriatly") {
      for {
        // Create Sample config files
        cwd     <- ZIO.succeed(Paths.get(java.lang.System.getProperty("user.dir")))
        homeDir <- ZIO.succeed(Paths.get(java.lang.System.getProperty("user.home")))
        _       <- createSampleConfigFiles(cwd, homeDir)

        // Check if the func checkAndGetOptionsFilePaths can
        config_args <- CliApp.loadOptionsFromConfigFiles("testApp")

        _ <- cleanUpSampleConfigFiles(cwd: Path, homeDir: Path)

      } yield assert(config_args)(hasSameElements(List("home=true", "dir=true", "home=false")))
    },
    test("should merge duplicate options") {
      val options       = List("option1=value1", "option2=value2", "option1=value3");
      val mergedOptions = CliApp.mergeOptionsBasedOnPriority(options);
      assert(mergedOptions)(equalTo(List("option1=value3", "option2=value2")));
    },
    test("should return directory ~/home and ./ which have .testApp config file for loading the args") {
      for {
        // Create Sample config files
        cwd     <- ZIO.succeed(Paths.get(java.lang.System.getProperty("user.dir")))
        homeDir <- ZIO.succeed(Paths.get(java.lang.System.getProperty("user.home")))
        _       <- createSampleConfigFiles(cwd, homeDir)

        // Check if the func checkAndGetOptionsFilePaths can
        paths <- CliApp.findPathsOfCliConfigFiles("testApp")

        _ <- cleanUpSampleConfigFiles(cwd: Path, homeDir: Path)

      } yield assert(paths)(hasSameElements(List(homeDir.toString(), cwd.toString())))
    }
  )

  def createSampleConfigFiles(cwd: Path, homeDir: Path): IO[IOException, Unit] =
    ZIO.attempt {
      Files.write(Paths.get(homeDir.toString(), ".testApp"), java.util.Arrays.asList("home=true"));
      Files.write(Paths.get(cwd.toString(), ".testApp"), java.util.Arrays.asList("dir=true\nhome=false"));

      ()
    }.refineToOrDie[IOException]

  def cleanUpSampleConfigFiles(cwd: Path, homeDir: Path): IO[IOException, Unit] =
    ZIO.attempt {
      Files.delete(Paths.get(homeDir.toString(), ".testApp"));
      Files.delete(Paths.get(cwd.toString(), ".testApp"));

      ()
    }.refineToOrDie[IOException]
}
