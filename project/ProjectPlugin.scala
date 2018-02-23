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
        Seq("-Ywarn-unused:patvars,imports,privates,locals")
      // explicits and (linted = imports,privates,locals,implicits) has far too
      // many false positives (implementations and implicit evidence)
      case _ => Nil
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

  override def projectSettings = Seq(
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test,
    scalacOptions in Test ++= {
      val dir = (baseDirectory in ThisBuild).value / "project"
      Seq(
        s"-Xmacro-settings:deriving.targets=$dir/deriving-targets.conf",
        s"-Xmacro-settings:deriving.defaults=$dir/deriving-defaults.conf"
      )
    },
    javaOptions in Test ++= {
      val settings =
        //Seq("-Ymacro-expand:discard") ++
        (scalacOptions in Test).value
          .filterNot(_.startsWith("-Yno-"))
          .filterNot(_.contains(","))
          .mkString(",")
      val classpath = (fullClasspath in Test).value.map(_.data).mkString(",")
      Seq(
        s"-Dpcplod.settings=$settings",
        s"-Dpcplod.classpath=$classpath"
      )
    },
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
      "-Yno-imports",
      "-Yno-predef",
      "-Xexperimental" // SAM types in 2.11
    ),
    // weird false positives...
    scalacOptions -= "-Ywarn-dead-code",
    scalacOptions ++= extraScalacOptions(scalaVersion.value),
    scalacOptions in (Compile, console) -= "-Xfatal-warnings",
    initialCommands in (Compile, console) := Seq(
      "java.lang.String",
      "scala.{Any,AnyRef,AnyVal,Boolean,Byte,Double,Float,Short,Int,Long,Char,Symbol,Unit,Null,Nothing,Option,Some,None,Either,Left,Right,StringContext}",
      "scala.annotation.tailrec",
      "scala.collection.immutable.{Map,Seq,List,::,Nil,Set,Vector}",
      "scala.util.{Try,Success,Failure}",
      "scala.Predef.{???,ArrowAssoc,identity,implicitly,<:<,=:=}",
      "shapeless.{ :: => :*:, _ }",
      "scalaz._",
      "Scalaz._"
    ).mkString("import ", ",", ""),
    // WORKAROUND: https://github.com/sbt/sbt/issues/3934
    compileOptions in (Test, compile) := {
      val oldOptions = (compileOptions in (Test, compile)).value
      val resDir     = (resourceDirectory in Test).value
      oldOptions.withClasspath(oldOptions.classpath :+ resDir)
    }
  )
}
