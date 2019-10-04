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
        case _ =>
          Nil
      }
    }
  def KindProjector =
    addCompilerPlugin(
      ("org.typelevel" %% "kind-projector" % "0.11.0").cross(CrossVersion.full)
    )

  def MonadicFor =
    addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")

  def SemanticDB =
    //addCompilerPlugin(scalafixSemanticdb)
    addCompilerPlugin(
      ("org.scalameta" % "semanticdb-scalac" % "4.2.3")
        .cross(CrossVersion.full)
    )

  def extraScalacOptions =
    Seq(
      "-Ywarn-extra-implicit",
      "-Ywarn-unused:explicits,patvars,imports,privates,locals,implicits",
      "-opt:l:method,inline",
      "-opt-inline-from:scala.**",
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
  override def trigger = allRequirements

  val autoImport = ProjectKeys
  import autoImport._

  val Scala213 = "2.13.1"

  override def buildSettings =
    Seq(
      organization := "org.scalaz",
      crossScalaVersions := Seq("2.12.10", Scala213),
      scalaVersion := Scala213,
      sonatypeGithost := (Github, "scalaz", "scalaz-deriving"),
      sonatypeDevelopers := List("Sam Halliday"),
      licenses := Seq(LGPL3),
      startYear := Some(2017),
      scalafmtConfig := file("project/scalafmt.conf"),
      scalafixConfig := Some(file("project/scalafix.conf"))
    )

  override def projectSettings =
    Seq(
      SemanticDB,
      libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % Test,
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
          case _ =>
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
      // WORKAROUND https://github.com/scalacenter/scalafix/issues/790
      scalacOptions += "-P:semanticdb:failures:warning",
      scalacOptions ++= extraScalacOptions,
      scalacOptions in (Test, doc) ~= (_.filterNot(_.startsWith("-Xlint"))),
      scalacOptions in (Test, doc) ~= (_.filterNot(_.startsWith("-Werror"))),
      scalacOptions in (Test, doc) ~= (_.filterNot(_.startsWith("-Ywarn"))),
      scalacOptions in (Test, doc) -= "-Xfatal-warnings",
      initialCommands in (Compile, console) := "import scalaz._, Scalaz._"
    )
}
