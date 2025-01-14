// Copyright: 2017 Sam Halliday
// License: http://www.gnu.org/licenses/gpl.html

import sbt._
import sbt.Keys._

import fommil.SensiblePlugin.autoImport._
import fommil.SonatypePlugin.autoImport._
import sbtdynver.DynVerPlugin.autoImport._
import org.scalafmt.sbt.ScalafmtPlugin
import scalafix.sbt.ScalafixPlugin, ScalafixPlugin.autoImport._

object ProjectKeys {
  val allScalaVersions = Seq(
    "2.12.15",
    "2.12.16",
    "2.12.17",
    "2.12.18",
    "2.12.19",
    "2.12.20",
    "2.13.10",
    "2.13.11",
    "2.13.12",
    "2.13.13",
    "2.13.14",
    "2.13.15",
    "2.13.16"
  )

  private[this] def latest(n: Int): String = {
    val prefix = "2." + n + "."
    prefix + allScalaVersions
      .filter(_.startsWith(prefix))
      .map(_.drop(prefix.length).toLong)
      .reduceLeftOption(_ max _)
      .getOrElse(
        sys.error(s"not found Scala ${prefix}x version $allScalaVersions")
      )
  }

  val Scala212 = latest(12)
  val Scala213 = latest(13)

  def MacroParadise =
    libraryDependencies ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, v)) if v <= 12 =>
          Seq(
            compilerPlugin(
              ("org.scalamacros" % "paradise" % "2.1.1")
                .cross(CrossVersion.full)
            )
          )
        case _                       =>
          Nil
      }
    }
  def KindProjector =
    addCompilerPlugin(
      ("org.typelevel" %% "kind-projector" % "0.13.3").cross(CrossVersion.full)
    )

  def MonadicFor =
    addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")

  def extraScalacOptions =
    Seq(
      "-Ywarn-extra-implicit",
      "-Ywarn-unused:explicits,patvars,imports,privates,locals,implicits"
    )
}

object ProjectPlugin extends AutoPlugin {

  override def requires =
    fommil.SensiblePlugin && fommil.SonatypePlugin && ScalafmtPlugin && ScalafixPlugin
  override def trigger  = allRequirements

  val autoImport = ProjectKeys
  import autoImport._

  override def buildSettings =
    Seq(
      organization       := "org.scalaz",
      crossScalaVersions := Seq(Scala212, Scala213),
      scalaVersion       := Scala213,
      sonatypeGithost    := (Github, "scalaz", "scalaz-deriving"),
      sonatypeDevelopers := List("Sam Halliday"),
      licenses           := Seq(LGPL3),
      startYear          := Some(2017)
    )

  override def projectSettings =
    Seq(
      publishTo                              := xerial.sbt.Sonatype.autoImport.sonatypePublishToBundle.value,
      libraryDependencies += {
        val v =
          CrossVersion.partialVersion(scalaVersion.value) match {
            case Some((2, 13)) =>
              if (
                SemanticSelector(">=2.13.13")
                  .matches(VersionNumber(scalaVersion.value))
              ) {
                "4.9.9"
              } else {
                "4.8.4"
              }
            case Some((2, 12)) =>
              if (
                SemanticSelector(">=2.12.19")
                  .matches(VersionNumber(scalaVersion.value))
              ) {
                "4.9.9"
              } else {
                "4.8.4"
              }
            case x             =>
              sys.error(s"unsupported version $x")
          }
        compilerPlugin(
          ("org.scalameta" % "semanticdb-scalac" % v).cross(CrossVersion.full)
        )
      },
      libraryDependencies += "org.scalatest" %% "scalatest-flatspec"       % "3.2.19" % Test,
      libraryDependencies += "org.scalatest" %% "scalatest-freespec"       % "3.2.19" % Test,
      libraryDependencies += "org.scalatest" %% "scalatest-shouldmatchers" % "3.2.19" % Test,
      scalacOptions --= Seq(
        "-Xfatal-warnings"
      ),
      scalacOptions --= {
        CrossVersion.partialVersion(scalaVersion.value) match {
          case Some((2, v)) if v >= 13 =>
            Seq(
              "-Xfuture",
              "-Yno-adapted-args",
              "-Ywarn-nullary-unit",
              "-Ywarn-nullary-override",
              "-Ywarn-inaccessible"
            )
          case _                       =>
            Nil
        }
      },
      scalacOptions ++= {
        CrossVersion.partialVersion(scalaVersion.value) match {
          case Some((2, n)) if n >= 13 =>
            Seq(
              "-Ymacro-annotations"
            )
          case Some((2, v)) if v <= 12 =>
            Seq(
              "-Ypartial-unification"
            )
        }
      },
      scalacOptions ++= Seq(
        "-language:experimental.macros,higherKinds,implicitConversions",
        "-unchecked",
        "-explaintypes",
        "-Xlog-free-terms",
        "-Xlog-free-types",
        "-Xlog-reflective-calls",
        "-Yrangepos"
      ),
      scalacOptions ++= extraScalacOptions,
      Test / doc / scalacOptions ~= (_.filterNot(_.startsWith("-Xlint"))),
      Test / doc / scalacOptions ~= (_.filterNot(_.startsWith("-Werror"))),
      Test / doc / scalacOptions ~= (_.filterNot(_.startsWith("-Ywarn"))),
      Test / doc / scalacOptions -= "-Xfatal-warnings",
      Compile / console / initialCommands    := "import scalaz._, Scalaz._"
    )
}
