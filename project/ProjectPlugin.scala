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
    addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.6")

  def extraScalacOptions(scalaVersion: String) =
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((2, 12)) =>
        Seq("-Ywarn-unused:explicits,patvars,imports,privates,locals,implicits")
      case _ => Nil
    }

  // WORKAROUND: https://github.com/sbt/sbt/issues/3934
  def resourcesOnCompilerCp(config: Configuration): Setting[_] =
    compileOptions in (config, compile) := {
      val oldOptions = (compileOptions in (config, compile)).value
      val resources  = (resourceDirectory in config).value
      oldOptions.withClasspath(resources +: oldOptions.classpath)
    }

}

object ProjectPlugin extends AutoPlugin {

  override def requires =
    fommil.SensiblePlugin && fommil.SonatypePlugin && ScalafmtPlugin
  override def trigger = allRequirements

  val autoImport = ProjectKeys
  import autoImport._

  override def buildSettings =
    Seq(
      organization := "com.fommil",
      crossScalaVersions := Seq("2.12.4", "2.11.12"),
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
      libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test,
      scalacOptions ++= Seq(
        "-language:_",
        "-unchecked",
        "-explaintypes",
        "-Ywarn-value-discard",
        "-Ywarn-numeric-widen",
        "-Ypartial-unification",
        "-Xlog-free-terms",
        "-Xlog-free-types",
        "-Xlog-reflective-calls",
        "-Yrangepos",
        "-Xexperimental" // SAM types in 2.11
      ),
      // weird false positives...
      //scalacOptions -= "-Ywarn-dead-code",
      scalacOptions ++= extraScalacOptions(scalaVersion.value),
      scalacOptions in (Compile, console) -= "-Xfatal-warnings",
      initialCommands in (Compile, console) := "import scalaz._, Scalaz._"
    )
}
