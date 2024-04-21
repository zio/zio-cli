import zio._
import zio.test._
import zio.test.Assertion._
import java.nio.file.{Files, Paths, Path}
import java.io.IOException
import zio.cli.ConfigFileArgsPlatformSpecific
import zio.cli.ConfigFilePlatformSpecific

object FileBasedArgs extends ZIOSpecDefault {

  val configFileOps: ConfigFilePlatformSpecific = ConfigFileArgsPlatformSpecific

  def spec = suite("FileBasedArgs")(
    test("should load options from files and merge them appropriatly") {
      for {
        // Create Sample config files
        cwd     <- ZIO.succeed(Paths.get(java.lang.System.getProperty("user.dir")))
        homeDir <- ZIO.succeed(Paths.get(java.lang.System.getProperty("user.home")))
        command <- ZIO.succeed("testApp")
        _       <- createSampleConfigFiles(cwd, homeDir, command)

        // Check if the func checkAndGetOptionsFilePaths can
        config_args <- configFileOps.loadOptionsFromConfigFiles(command)

        _ <- cleanUpSampleConfigFiles(cwd: Path, homeDir: Path, command)

      } yield assert(config_args)(hasSameElements(List("home=true", "dir=true", "home=false")))
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

  def createSampleConfigFiles(cwd: Path, homeDir: Path, command: String = "testApp"): IO[IOException, Unit] =
    ZIO.attempt {
      Files.write(Paths.get(homeDir.toString(), s".$command"), java.util.Arrays.asList("home=true"));
      Files.write(Paths.get(cwd.toString(), s".$command"), java.util.Arrays.asList("dir=true\nhome=false"));

      ()
    }.refineToOrDie[IOException]

  def cleanUpSampleConfigFiles(cwd: Path, homeDir: Path, command: String = "testApp"): IO[IOException, Unit] =
    ZIO.attempt {
      Files.delete(Paths.get(homeDir.toString(), s".$command"));
      Files.delete(Paths.get(cwd.toString(), s".$command"));

      ()
    }.refineToOrDie[IOException]
}
