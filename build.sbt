inThisBuild(
  Seq(
    organization := "com.fommil",
    crossScalaVersions := Seq("2.12.3", "2.11.11"),
    scalaVersion := crossScalaVersions.value.head,
    sonatypeGithub := ("fommil", "stalactite"),
    licenses := Seq(LGPL3)
  )
)

libraryDependencies ++= Seq(
  "org.scala-lang"         % "scala-compiler" % scalaVersion.value % "provided",
  "org.scala-lang"         % "scala-reflect"  % scalaVersion.value % "provided",
  "org.scala-lang.modules" %% "scala-xml"     % "1.0.6"            % "test",
  "org.ensime"             %% "pcplod"        % "1.2.1"            % "test",
  "com.github.mpilquist"   %% "simulacrum"    % "0.11.0"           % "test",
  "com.chuusai"            %% "shapeless"     % "2.3.2"            % "test",
  "org.typelevel"          %% "export-hook"   % "1.2.0"            % "test",
  "com.typesafe.play"      %% "play-json"     % "2.6.3"            % "test",
  "org.scalaz"             %% "scalaz-core"   % "7.2.15"           % "test"
)

scalacOptions in Test += {
  val custom = Map(
    "stalactite.typeclasses.Cobar" -> "stalactite.typeclasses.CustomCobar.go"
  ).map { case (from, to) => s"$from=$to" }.mkString("|", "|", "|")
  s"-Xmacro-settings:stalactite=$custom"
}
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
  "-Yno-predef"
)

scalacOptions := scalacOptions.value.filterNot(_.startsWith("-Ywarn-unused"))

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
scalafmtVersion in ThisBuild := "1.2.0"

scalacOptions in (Compile, console) -= "-Xfatal-warnings"
initialCommands in (Compile, console) := Seq(
  "java.lang.String",
  "scala.{Any,AnyRef,AnyVal,Boolean,Byte,Double,Float,Short,Int,Long,Char,Symbol,Unit,Null,Nothing,Option,Some,None,Either,Left,Right,StringContext}",
  "scala.annotation.tailrec",
  "scala.collection.immutable.{Map,Seq,List,::,Nil,Set,Vector}",
  "scala.util.{Try,Success,Failure}",
  "scala.Predef.{???,ArrowAssoc,identity,implicitly,<:<,=:=}",
  "shapeless.{ :: => :*:, _ }",
  "_root_.io.circe",
  "scalaz._",
  "Scalaz._"
).mkString("import ", ",", "")

addCommandAlias("fmt", ";sbt:scalafmt ;scalafmt ;test:scalafmt")
