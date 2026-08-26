import sbt._
import sbt.Keys._

import sbtcrossproject.CrossPlugin.autoImport._
import sbtbuildinfo._
import BuildInfoKeys._
// `BuildInfoKey` has to come from here rather than the bare `sbtbuildinfo` package: on the
// Scala 3 / sbt2 axis, `sbtbuildinfo.BuildInfoKey` is only an object (no type alias), and only
// `BuildInfoPlugin.autoImport` pairs a `type BuildInfoKey` with it, uniformly on both axes.
import sbtbuildinfo.BuildInfoPlugin.autoImport.BuildInfoKey
import scalafix.sbt.ScalafixPlugin.autoImport.scalafixSemanticdb

object BuildHelper {

  val Scala212 = "2.12.21"
  val Scala213 = "2.13.18"
  val Scala3   = "3.3.8"

  def buildInfoSettings(packageName: String) =
    Seq(
      // Wrapped explicitly with `BuildInfoKey(...)` rather than relying on its implicit
      // `SettingKey[T] => BuildInfoKey` conversion applying automatically inside a `Seq[BuildInfoKey](...)`
      // literal: Scala 3 does not apply it there the way Scala 2 does.
      buildInfoKeys := Seq[BuildInfoKey](
        BuildInfoKey(name),
        BuildInfoKey(version),
        BuildInfoKey(scalaVersion),
        BuildInfoKey(sbtVersion),
        BuildInfoKey(isSnapshot)
      ),
      buildInfoPackage := packageName,
      buildInfoObject  := "BuildInfo"
    )

  private val stdOptions = Seq(
    "-deprecation",
    "-encoding",
    "UTF-8",
    "-feature",
    "-unchecked"
  )

  private val std2xOptions = Seq(
    "-language:higherKinds",
    "-language:existentials",
    "-explaintypes",
    "-Yrangepos",
    "-Xlint:_,-missing-interpolator,-type-parameter-shadow",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard"
  ) ++ customOptions

  private def propertyFlag(property: String, default: Boolean) =
    sys.props.get(property).map(_.toBoolean).getOrElse(default)

  private def customOptions =
    if (propertyFlag("fatal.warnings", true)) {
      Seq("-Xfatal-warnings")
    } else {
      Nil
    }

  def extraOptions(scalaVersion: String, optimize: Boolean) =
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((3, _)) =>
        Seq(
          "-noindent"
        )
      case Some((2, 13)) =>
        Seq(
          "-Ywarn-unused:params,-implicits"
        ) ++ std2xOptions
      case Some((2, 12)) =>
        Seq(
          "-opt-warnings",
          "-Ywarn-extra-implicit",
          "-Ywarn-unused:_,imports",
          "-Ywarn-unused:imports",
          "-Ypartial-unification",
          "-Yno-adapted-args",
          "-Ywarn-inaccessible",
          "-Ywarn-infer-any",
          "-Ywarn-nullary-override",
          "-Ywarn-nullary-unit",
          "-Ywarn-unused:params,-implicits",
          "-Xfuture",
          "-Xsource:2.13",
          "-Xmax-classfile-name",
          "242"
        ) ++ std2xOptions
      case _ => Seq.empty
    }

  def platformSpecificSources(platform: String, conf: String, baseDirectory: File)(versions: String*) =
    for {
      platform <- List("shared", platform)
      version  <- "scala" :: versions.toList.map("scala-" + _)
      result    = baseDirectory.getParentFile / platform.toLowerCase / "src" / conf / version
      if result.exists
    } yield result

  def crossPlatformSources(scalaVer: String, platform: String, conf: String, baseDir: File) = {
    val versions = CrossVersion.partialVersion(scalaVer) match {
      case Some((2, 12)) =>
        List("2.12", "2.11+", "2.12+", "2.11-2.12", "2.12-2.13", "2.x")
      case Some((2, 13)) =>
        List("2.13", "2.11+", "2.12+", "2.13+", "2.12-2.13", "2.x")
      case Some((3, 0)) =>
        List("dotty", "2.11+", "2.12+", "2.13+", "3.x")
      case _ =>
        List()
    }
    platformSpecificSources(platform, conf, baseDir)(versions: _*)
  }

  lazy val crossProjectSettings = Seq(
    Compile / unmanagedSourceDirectories ++= {
      crossPlatformSources(
        scalaVersion.value,
        crossProjectPlatform.value.identifier,
        "main",
        baseDirectory.value
      )
    },
    Test / unmanagedSourceDirectories ++= {
      crossPlatformSources(
        scalaVersion.value,
        crossProjectPlatform.value.identifier,
        "test",
        baseDirectory.value
      )
    }
  )

  def stdSettings(prjName: String) = Seq(
    name                     := s"$prjName",
    crossScalaVersions       := Seq(Scala212, Scala213, Scala3),
    ThisBuild / scalaVersion := Scala213,
    scalacOptions ++= stdOptions ++ extraOptions(scalaVersion.value, optimize = !isSnapshot.value),
    semanticdbEnabled := scalaVersion.value == Scala213, // enable SemanticDB
    semanticdbOptions += "-P:semanticdb:synthetics:on",
    semanticdbVersion        := scalafixSemanticdb.revision, // use Scalafix compatible version
    Test / parallelExecution := scalaVersion.value != Scala3,
    incOptions ~= (_.withLogRecompileOnMacro(false))
  )

  implicit class ModuleHelper(p: Project) {
    def module: Project = p.in(file(p.id)).settings(stdSettings(p.id))
  }
}
