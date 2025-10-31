import BuildHelper._
import explicitdeps.ExplicitDepsPlugin.autoImport.moduleFilterRemoveValue
import sbt.addSbtPlugin
import sbtcrossproject.CrossPlugin.autoImport.crossProject

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
    )
  )
)

addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")
addCommandAlias("check", "all scalafmtSbtCheck scalafmtCheck test:scalafmtCheck")

val zioVersion           = "2.1.22"
val zioJsonVersion       = "0.7.44"
val scalaJavaTimeVersion = "2.6.0"

lazy val root = project
  .in(file("."))
  .settings(
    publish / skip := true,
    unusedCompileDependenciesFilter -= moduleFilter("org.scala-js", "scalajs-library"),
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

lazy val zioCli = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("zio-cli"))
  .settings(stdSettings("zio-cli"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.cli"))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %%% "zio"          % zioVersion,
      "dev.zio" %%% "zio-json"     % zioJsonVersion,
      "dev.zio" %%% "zio-streams"  % zioVersion,
      "dev.zio" %%% "zio-test"     % zioVersion % Test,
      "dev.zio" %%% "zio-test-sbt" % zioVersion % Test
    )
  )
  .jvmSettings(
    libraryDependencies += "dev.zio" %% "zio-process" % "0.7.1"
  )
  .nativeSettings(Test / fork := false)
  .nativeSettings(
    libraryDependencies += "io.github.cquiroz" %%% "scala-java-time" % scalaJavaTimeVersion % Test
  )
  .jsSettings(
    libraryDependencies += "io.github.cquiroz" %%% "scala-java-time" % scalaJavaTimeVersion % Test
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
    addSbtPlugin("org.scalameta" %% "sbt-native-image" % "0.3.2")
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
      "dev.zio" %% "zio-test"          % zioVersion,
      "dev.zio" %% "zio-test-sbt"      % zioVersion,
      "dev.zio" %% "zio-test-magnolia" % zioVersion
    )
  )
  .nativeSettings(Test / fork := false)
  .nativeSettings(
    libraryDependencies += "io.github.cquiroz" %%% "scala-java-time" % scalaJavaTimeVersion % Test
  )
  .jsSettings(
    libraryDependencies += "io.github.cquiroz" %%% "scala-java-time" % scalaJavaTimeVersion % Test
  )
  .jsSettings(scalaJSUseMainModuleInitializer := true)
  .dependsOn(zioCli)
Global / onChangedBuildSource := ReloadOnSourceChanges
