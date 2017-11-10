inThisBuild(
  Seq(
    organization := "com.fommil",
    crossScalaVersions := Seq("2.12.4", "2.11.11"),
    scalaVersion := crossScalaVersions.value.head,
    sonatypeGithost := (Gitlab, "fommil", "stalactite"),
    licenses := Seq(LGPL3)
  )
)

sonatypeDevelopers := List("Sam Halliday")

enablePlugins(NeoJmh)
inConfig(Jmh)(sensibleTestSettings ++ scalafmtSettings)
headerSettings(Jmh)

libraryDependencies ++= Seq(
  "com.chuusai"            %% "shapeless"     % "2.3.2",
  "org.scalaz"             %% "scalaz-core"   % "7.2.16",
  "org.scala-lang"         % "scala-compiler" % scalaVersion.value % "provided",
  "org.scala-lang"         % "scala-reflect"  % scalaVersion.value % "provided",
  "org.scala-lang.modules" %% "scala-xml"     % "1.0.6" % "test",
  "org.ensime"             %% "pcplod"        % "1.2.1" % "test",
  "com.github.mpilquist"   %% "simulacrum"    % "0.11.0" % "test",
  "org.typelevel"          %% "export-hook"   % "1.2.0" % "test",
  "com.typesafe.play"      %% "play-json"     % "2.6.7" % "test"
)

libraryDependencies += "io.frees" %% "iotaz-core" % "0.3.2"

scalacOptions in Test ++= {
  val dir = (baseDirectory in ThisBuild).value / "project"
  Seq(
    s"-Xmacro-settings:stalactite.targets=$dir/stalactite-targets.conf",
    s"-Xmacro-settings:stalactite.defaults=$dir/stalactite-defaults.conf"
  )
}

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")

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
}

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
)

// weird false positives...
scalacOptions -= "-Ywarn-dead-code"
scalacOptions in (Compile, compile) ++= {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, 12)) if sys.env.get("CI").isDefined =>
      // very annoying during local dev
      Seq("-Ywarn-unused:explicits,patvars,linted")
    case _ => Nil
  }
}

addCompilerPlugin(
  "org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full
)

wartremoverWarnings in (Compile, compile) := Seq(
  Wart.FinalCaseClass,
  Wart.ExplicitImplicitTypes
)
wartremoverWarnings in (Test, compile) := Seq(
  Wart.FinalCaseClass
)

scalafmtOnCompile in ThisBuild := true
scalafmtConfig in ThisBuild := file("project/scalafmt.conf")
scalafmtVersion in ThisBuild := "1.3.0"

scalacOptions in (Compile, console) -= "-Xfatal-warnings"
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
).mkString("import ", ",", "")

addCommandAlias("fmt", ";sbt:scalafmt ;scalafmt ;test:scalafmt ;jmh:scalafmt")
