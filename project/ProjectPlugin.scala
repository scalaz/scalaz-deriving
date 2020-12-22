// Copyright: 2017 Sam Halliday
// License: http://www.gnu.org/licenses/gpl.html

import sbt._
import sbt.Keys._

import fommil.SensiblePlugin.autoImport._
import fommil.SonatypePlugin.autoImport._
import sbtdynver.DynVerPlugin.autoImport._
import org.scalafmt.sbt.ScalafmtPlugin, ScalafmtPlugin.autoImport._
import scalafix.sbt.ScalafixPlugin, ScalafixPlugin.autoImport._

object ProjectKeys {
  val allScalaVersions = Seq(
    "2.12.8",
    "2.12.9",
    "2.12.10",
    "2.12.11",
    "2.12.12",
    "2.13.0",
    "2.13.1",
    "2.13.2",
    "2.13.3",
    "2.13.4"
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
      ("org.typelevel" %% "kind-projector" % "0.11.2").cross(CrossVersion.full)
    )

  def MonadicFor =
    addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")

  def SemanticDB =
    //addCompilerPlugin(scalafixSemanticdb)
    addCompilerPlugin(
      ("org.scalameta" % "semanticdb-scalac" % "4.4.3")
        .cross(CrossVersion.full)
    )

  def extraScalacOptions =
    Seq(
      "-Ywarn-extra-implicit",
      "-Ywarn-unused:explicits,patvars,imports,privates,locals,implicits",
      "-opt:l:method,inline",
      "-opt-inline-from:scalaz.**",
      "-opt-inline-from:fommil.**",
      "-opt-inline-from:magnolia.**",
      "-opt-inline-from:shapeless.**",
      "-opt-inline-from:jsonformat.**",
      "-opt-inline-from:xmlformat.**"
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
      startYear := Some(2017),
      scalafmtConfig := file("project/scalafmt.conf")
    )

  override def projectSettings =
    Seq(
      SemanticDB,
      libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.3" % Test,
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
      scalacOptions in (Test, doc) ~= (_.filterNot(_.startsWith("-Xlint"))),
      scalacOptions in (Test, doc) ~= (_.filterNot(_.startsWith("-Werror"))),
      scalacOptions in (Test, doc) ~= (_.filterNot(_.startsWith("-Ywarn"))),
      scalacOptions in (Test, doc) -= "-Xfatal-warnings",
      initialCommands in (Compile, console) := "import scalaz._, Scalaz._"
    )
}
