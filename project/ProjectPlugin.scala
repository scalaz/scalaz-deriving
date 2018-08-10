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
    addCompilerPlugin(
      ("org.scalamacros" % "paradise" % "2.1.1").cross(CrossVersion.full)
    )
  def KindProjector =
    addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.7")

  def MonadicFor =
    addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.2.4")

  def SemanticDB =
    addCompilerPlugin(
      ("org.scalameta" % "semanticdb-scalac" % "4.0.0-M7")
        .cross(CrossVersion.full)
    )

  def extraScalacOptions(scalaVersion: String) =
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((2, 12)) =>
        Seq(
          "-Ywarn-extra-implicit",
          "-Ywarn-unused:explicits,patvars,imports,privates,locals,implicits",
          "-opt:l:method,inline",
          "-opt-inline-from:scala.**",
          "-opt-inline-from:scalaz.**",
          "-opt-inline-from:fommil.**",
          "-opt-inline-from:iotaz.**",
          "-opt-inline-from:magnolia.**",
          "-opt-inline-from:shapeless.**",
          "-opt-inline-from:jsonformat.**",
          "-opt-inline-from:xmlformat.**"
        )
      case _ =>
        Seq(
          "-Xexperimental"
        )
    }
}

object ProjectPlugin extends AutoPlugin {

  override def requires =
    fommil.SensiblePlugin && fommil.SonatypePlugin && ScalafmtPlugin && ScalafixPlugin
  override def trigger = allRequirements

  val autoImport = ProjectKeys
  import autoImport._

  override def buildSettings =
    Seq(
      organization := "com.fommil",
      crossScalaVersions := Seq("2.12.6", "2.11.12"),
      scalaVersion := crossScalaVersions.value.head,
      sonatypeGithost := (Gitlab, "fommil", "scalaz-deriving"),
      sonatypeDevelopers := List("Sam Halliday"),
      licenses := Seq(LGPL3),
      startYear := Some(2017),
      scalafmtConfig := Some(file("project/scalafmt.conf")),
      scalafixConfig := Some(file("project/scalafix.conf"))
    )

  override def projectSettings =
    Seq(
      SemanticDB,
      libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test,
      scalacOptions ++= Seq(
        "-language:experimental.macros,higherKinds,implicitConversions",
        "-unchecked",
        "-explaintypes",
        "-Ypartial-unification",
        "-Xlog-free-terms",
        "-Xlog-free-types",
        "-Xlog-reflective-calls",
        "-Yrangepos"
      ),
      scalacOptions := {
        // WORKAROUND https://github.com/propensive/magnolia/pull/132
        val old = scalacOptions.value
        val bin = scalaBinaryVersion.value
        if (bin == "2.11") old.filterNot(_ == "-Xfatal-warnings")
        else old
      },
      scalacOptions ++= extraScalacOptions(scalaVersion.value),
      scalacOptions in (Test, doc) ~= (_.filterNot(_.startsWith("-Xlint"))),
      scalacOptions in (Test, doc) ~= (_.filterNot(_.startsWith("-Werror"))),
      scalacOptions in (Test, doc) ~= (_.filterNot(_.startsWith("-Ywarn"))),
      scalacOptions in (Test, doc) -= "-Xfatal-warnings",
      initialCommands in (Compile, console) := "import scalaz._, Scalaz._"
    )
}
