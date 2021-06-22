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
    "2.12.8",
    "2.12.9",
    "2.12.10",
    "2.12.11",
    "2.12.12",
    "2.12.13",
    "2.12.14",
    "2.13.0",
    "2.13.1",
    "2.13.2",
    "2.13.3",
    "2.13.4",
    "2.13.5",
    "2.13.6"
  )

  private[this] def latest(n: Int): String = {
    val prefix = "2." + n + "."
    prefix + allScalaVersions
      .filter(_.startsWith(prefix))
      .map(_.drop(prefix.length).toLong)
      .reduceLeftOption(_ max _)
      .getOrElse(
        sys.error(s"not found Scala ${prefix}x version ${allScalaVersions}")
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
      ("org.typelevel" %% "kind-projector" % "0.13.0").cross(CrossVersion.full)
    )

  def MonadicFor =
    addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")

  def SemanticDB =
    //addCompilerPlugin(scalafixSemanticdb)
    addCompilerPlugin(
      ("org.scalameta" % "semanticdb-scalac" % "4.4.22")
        .cross(CrossVersion.full)
    )

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
      organization := "org.scalaz",
      crossScalaVersions := Seq(Scala212, Scala213),
      scalaVersion := Scala213,
      sonatypeGithost := (Github, "scalaz", "scalaz-deriving"),
      sonatypeDevelopers := List("Sam Halliday"),
      licenses := Seq(LGPL3),
      startYear := Some(2017)
    )

  override def projectSettings =
    Seq(
      publishTo := xerial.sbt.Sonatype.autoImport.sonatypePublishToBundle.value,
      SemanticDB,
      libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % Test,
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
      Compile / console / initialCommands := "import scalaz._, Scalaz._"
    )
}
