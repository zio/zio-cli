import BuildHelper._
import sbt.addSbtPlugin
import sbtcrossproject.CrossPlugin.autoImport.crossProject
import zio.sbt.ZioSbtCiPlugin._
import zio.sbt.githubactions.{Condition, Job, Step, Strategy}

inThisBuild(
  List(
    organization := "dev.zio",
    homepage     := Some(url("https://zio.dev/zio-cli/")),
    licenses     := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers   := List(
      Developer(
        "jdegoes",
        "John De Goes",
        "john@degoes.net",
        url("http://degoes.net")
      )
    ),
    pgpPassphrase := sys.env.get("PGP_PASSWORD").map(_.toArray),
    pgpPublicRing := file("/tmp/public.asc"),
    pgpSecretRing := file("/tmp/secret.asc"),
    scmInfo       := Some(
      ScmInfo(url("https://github.com/zio/zio-cli/"), "scm:git:git@github.com:zio/zio-cli.git")
    ),

    // zio-sbt-ci settings: keep the generated ci.yml matching the CI shape this project already
    // had (master-only, JDK 21 by default, the same JVM memory flags, and the old two-job test
    // split) rather than the plugin's stock defaults.
    ciEnabledBranches    := Seq("master"),
    ciDefaultJavaVersion := "21",
    ciJvmOptions         := Seq(
      "-Xms6G",
      "-Xmx6G",
      "-Xss4M",
      "-XX:+UseG1GC",
      "-XX:ReservedCodeCacheSize=512M",
      "-XX:NonProfiledCodeHeapSize=256M"
    ),
    ciWorkflowEnv := {
      val opts = ("-XX:+PrintCommandLineFlags" +: ciJvmOptions.value).mkString(" ")
      Map("JDK_JAVA_OPTIONS" -> opts, "SBT_OPTS" -> opts)
    },
    ciUpdateReadmeCondition := Some(
      Condition.Expression("github.event_name == 'push'") ||
        Condition.Expression("github.event_name == 'workflow_dispatch'") ||
        (Condition.Expression("github.event_name == 'release'") && Condition.Expression(
          "github.event.action == 'published'"
        ))
    )
  )
)

addCommandAlias("fmt", "all scalafmtSbt scalafmt Test/scalafmt")
addCommandAlias("check", "all scalafmtSbtCheck scalafmtCheck Test/scalafmtCheck")
addCommandAlias("lint", "check")

val zioVersion           = "2.1.26"
val zioJsonVersion       = "1.0.0"
val scalaJavaTimeVersion = "2.7.0"

// sbt-scala-native 0.5.12's own test-interface is newer than the one zio-test-sbt 2.1.26 depends
// on (0.5.10); both are 0.5.x, so the newer one coursier already picks is fine - this just tells
// sbt's strict conflict manager that's an acceptable substitution instead of a hard error. Every
// cross-built Scala version gets its own suffixed artifact name, so all three need an entry.
ThisBuild / libraryDependencySchemes ++= Seq(
  "org.scala-native" % "test-interface_native0.5_2.12" % VersionScheme.Always,
  "org.scala-native" % "test-interface_native0.5_2.13" % VersionScheme.Always,
  "org.scala-native" % "test-interface_native0.5_3"    % VersionScheme.Always
)

lazy val root = project
  .in(file("."))
  .settings(
    publish / skip     := true,
    crossScalaVersions := Nil
  )
  .aggregate(
    zioCli.jvm,
    zioCli.js,
    zioCli.native,
    examples.jvm,
    examples.js,
    examples.native,
    docs,
    sbtZioCli,
    testkit.jvm,
    testkit.js,
    testkit.native
  )

inThisBuild(
  List(
    ciTargetScalaVersions := targetScalaVersionsFor(zioCli.jvm, zioCli.js, zioCli.native).value,

    // Preserves the old two-job split (all platforms on JDK 17, JVM-only re-run on 21/25)
    // instead of the plugin's default of testing every platform on every configured JDK.
    ciTestJobs := Seq(
      Job(
        id = "testCross",
        name = "Test",
        strategy = Some(
          Strategy(
            matrix = Map(
              "java"     -> List("17"),
              "scala"    -> List("2.12.x", "2.13.x", "3.3.x"),
              "platform" -> List("JVM", "JS", "Native")
            ),
            failFast = false
          )
        ),
        steps =
          Seq(
            SetupJava("${{ matrix.java }}"),
            SetupSBT,
            CacheDependencies,
            Checkout.value,
            Step.SingleStep(
              name = "Install libuv",
              condition = Some(Condition.Expression("matrix.platform == 'Native'")),
              run = Some("sudo apt-get update && sudo apt-get install -y libuv1-dev")
            ),
            Step.SingleStep(
              name = "Run tests",
              run = Some("sbt ++${{ matrix.scala }} zioCli${{ matrix.platform }}/test")
            )
          )
      ),
      Job(
        id = "testJVMs",
        name = "Test JVMs",
        strategy = Some(
          Strategy(
            matrix = Map(
              "java"  -> List("21", "25"),
              "scala" -> List("2.12.x", "2.13.x", "3.3.x")
            ),
            failFast = false
          )
        ),
        steps = Seq(
          SetupJava("${{ matrix.java }}"),
          SetupSBT,
          CacheDependencies,
          Checkout.value,
          Step.SingleStep(
            name = "Run tests",
            run = Some("sbt ++${{ matrix.scala }} zioCliJVM/test")
          )
        )
      )
    )
  )
)

lazy val zioCli = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("zio-cli"))
  .settings(stdSettings("zio-cli"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.cli"))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio"          % zioVersion,
      "dev.zio" %% "zio-json"     % zioJsonVersion,
      "dev.zio" %% "zio-streams"  % zioVersion,
      "dev.zio" %% "zio-test"     % zioVersion % Test,
      "dev.zio" %% "zio-test-sbt" % zioVersion % Test
    )
  )
  .jvmSettings(
    libraryDependencies += "dev.zio" %% "zio-process" % "0.7.1"
  )
  .nativeSettings(Test / fork := false)
  .nativeSettings(
    libraryDependencies += "io.github.cquiroz" %% "scala-java-time" % scalaJavaTimeVersion % Test
  )
  .jsSettings(
    libraryDependencies += "io.github.cquiroz" %% "scala-java-time" % scalaJavaTimeVersion % Test
  )
  .jsSettings(scalaJSUseMainModuleInitializer := true)
  .settings(testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"))

lazy val examples = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("examples"))
  .settings(stdSettings("examples"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.cli.examples"))
  .settings(
    skip / publish := true,
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-streams" % zioVersion
    )
  )
  .jsSettings(scalaJSUseMainModuleInitializer := true)
  .dependsOn(zioCli)

lazy val docs = project
  .in(file("zio-cli-docs"))
  .settings(
    moduleName := "zio-cli-docs",
    scalacOptions -= "-Yno-imports",
    scalacOptions -= "-Xfatal-warnings",
    libraryDependencies ++= Seq("dev.zio" %% "zio" % zioVersion),
    ScalaUnidoc / unidoc / unidocProjectFilter := inProjects(zioCli.jvm),
    projectName                                := "ZIO CLI",
    mainModuleName                             := (zioCli.jvm / moduleName).value,
    projectStage                               := ProjectStage.Experimental,
    ScalaUnidoc / unidoc / unidocProjectFilter := inProjects(zioCli.jvm)
  )
  .dependsOn(zioCli.jvm)
  .enablePlugins(WebsitePlugin)

lazy val sbtZioCli = project
  .in(file("sbt-zio-cli"))
  .settings(
    name               := "sbt-zio-cli",
    organization       := "zio.cli.sbt",
    scalaVersion       := Scala212,
    crossScalaVersions := Seq(Scala212),
    publish / skip     := true,
    addSbtPlugin("org.scalameta" %% "sbt-native-image" % "0.5.0")
  )
  .enablePlugins(SbtPlugin)

lazy val testkit = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("zio-cli-testkit"))
  .settings(stdSettings("zio-cli-testkit"))
  .settings(buildInfoSettings("zio.cli.testkit"))
  .settings(testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"))
  .settings(skip / publish := true)
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-test"     % zioVersion,
      "dev.zio" %% "zio-test-sbt" % zioVersion
    )
  )
  // zio-test-magnolia has never published a Scala Native artifact (JVM/JS only), so it can't be a
  // cross-platform dependency here.
  .jvmSettings(
    libraryDependencies += "dev.zio" %% "zio-test-magnolia" % zioVersion
  )
  .jsSettings(
    libraryDependencies += "dev.zio" %% "zio-test-magnolia" % zioVersion
  )
  .nativeSettings(Test / fork := false)
  .nativeSettings(
    libraryDependencies += "io.github.cquiroz" %% "scala-java-time" % scalaJavaTimeVersion % Test
  )
  .jsSettings(
    libraryDependencies += "io.github.cquiroz" %% "scala-java-time" % scalaJavaTimeVersion % Test
  )
  .jsSettings(scalaJSUseMainModuleInitializer := true)
  .dependsOn(zioCli)
Global / onChangedBuildSource := ReloadOnSourceChanges
