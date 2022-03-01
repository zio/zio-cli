import BuildHelper._
import explicitdeps.ExplicitDepsPlugin.autoImport.moduleFilterRemoveValue
import sbt.addSbtPlugin
import sbtcrossproject.CrossPlugin.autoImport.crossProject

inThisBuild(
  List(
    organization := "dev.zio",
    homepage := Some(url("https://zio.github.io/zio-cli/")),
    licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers := List(
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
    scmInfo := Some(
      ScmInfo(url("https://github.com/zio/zio-cli/"), "scm:git:git@github.com:zio/zio-cli.git")
    )
  )
)

addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")
addCommandAlias("check", "all scalafmtSbtCheck scalafmtCheck test:scalafmtCheck")

val zioVersion = "2.0.0-RC2"

lazy val root = project
  .in(file("."))
  .settings(
    skip in publish := true,
    unusedCompileDependenciesFilter -= moduleFilter("org.scala-js", "scalajs-library")
  )
  .aggregate(
    zioCliJVM,
    zioCliJS,
    examplesJVM,
    examplesJS
  )

lazy val zioCli = crossProject(JSPlatform, JVMPlatform)
  .in(file("zio-cli"))
  .settings(stdSettings("zio-cli"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.cli"))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio"          % zioVersion,
      "dev.zio" %% "zio-test"     % zioVersion % "test",
      "dev.zio" %% "zio-test-sbt" % zioVersion % "test"
    )
  )
  .settings(testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"))

lazy val zioCliJVM = zioCli.jvm
  .settings(dottySettings)

lazy val zioCliJS = zioCli.js
  .settings(scalaJSUseMainModuleInitializer := true)

lazy val examplesJVM = examples.jvm
  .settings(dottySettings)

lazy val examplesJS = examples.js
  .settings(scalaJSUseMainModuleInitializer := true)

lazy val examples = crossProject(JSPlatform, JVMPlatform)
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
  .dependsOn(zioCli)

lazy val docs = project
  .in(file("zio-cli-docs"))
  .settings(
    skip / publish := true,
    moduleName := "zio-cli-docs",
    scalacOptions -= "-Yno-imports",
    scalacOptions -= "-Xfatal-warnings",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % zioVersion
    ),
    unidocProjectFilter in (ScalaUnidoc, unidoc) := inProjects(root),
    target in (ScalaUnidoc, unidoc) := (baseDirectory in LocalRootProject).value / "website" / "static" / "api",
    cleanFiles += (target in (ScalaUnidoc, unidoc)).value,
    docusaurusCreateSite := docusaurusCreateSite.dependsOn(unidoc in Compile).value,
    docusaurusPublishGhpages := docusaurusPublishGhpages.dependsOn(unidoc in Compile).value
  )
  .dependsOn(root)
  .enablePlugins(MdocPlugin, DocusaurusPlugin, ScalaUnidocPlugin)

lazy val sbtZioCli = project
  .in(file("sbt-zio-cli"))
  .settings(
    name := "sbt-zio-cli",
    organization := "zio.cli.sbt",
    scalaVersion := "2.12.14",
    version := "0.0.0-SNAPSHOT",
    addSbtPlugin("org.scalameta" %% "sbt-native-image" % "0.2.2")
  )
  .enablePlugins(SbtPlugin)
