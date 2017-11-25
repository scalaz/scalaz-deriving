val deriving = (project in file("deriving-macro")).settings(
  name := "deriving-macro",
  MacroParadise,
  libraryDependencies ++= Seq(
    "org.scala-lang"       % "scala-compiler" % scalaVersion.value % "provided",
    "org.scala-lang"       % "scala-reflect"  % scalaVersion.value % "provided",
    "org.scalaz"           %% "scalaz-core"   % "7.2.16"           % "test",
    "com.chuusai"          %% "shapeless"     % "2.3.2"            % "test",
    "org.ensime"           %% "pcplod"        % "1.2.1"            % "test",
    "com.github.mpilquist" %% "simulacrum"    % "0.11.0"           % "test",
    "com.typesafe.play"    %% "play-json"     % "2.6.7"            % "test"
  )
)

// extensions to scalaz7.2
val scalaz = (project in file("scalaz-deriving-base")).settings(
  KindProjector,
  name := "scalaz-deriving-base",
  licenses := Seq(
    ("BSD-3" -> url("https://opensource.org/licenses/BSD-3-Clause"))
  ),
  libraryDependencies ++= Seq(
    "com.chuusai" %% "shapeless"   % "2.3.2",
    "org.scalaz"  %% "scalaz-core" % "7.2.16"
  )
)

val derivez = (project in file("scalaz-deriving"))
  .enablePlugins(NeoJmhPlugin)
  .dependsOn(
    scalaz,
    deriving % "test"
  )
  .settings(
    KindProjector,
    MacroParadise,
    name := "scalaz-deriving",
    envVars in Jmh += ("RANDOM_DATA_GENERATOR_SEED" -> "0"),
    envVars in NeoJmhPlugin.JmhInternal := (envVars in Jmh).value,
    libraryDependencies ++= Seq(
      "org.scala-lang"      % "scala-compiler"         % scalaVersion.value % "provided",
      "io.frees"            %% "iotaz-core"            % "0.3.2",
      "com.danielasfregola" %% "random-data-generator" % "2.2" % "test,jmh"
    )
  )

val xmlformat = (project in file("examples/xmlformat"))
  .dependsOn(deriving)
  .settings(
    MacroParadise,
    scalacOptions -= "-Yno-imports",
    libraryDependencies ++= Seq(
      "org.scalaz"             %% "scalaz-core" % "7.2.16",
      "com.chuusai"            %% "shapeless"   % "2.3.2",
      "com.github.mpilquist"   %% "simulacrum"  % "0.11.0",
      "org.scala-lang.modules" %% "scala-xml"   % "1.0.6"
    )
  )

// root project
publishLocal := {}
publish := {}

// WORKAROUND: until https://github.com/scalameta/scalafmt/issues/1081
commands += Command.args("fmt", "scalafmt CLI") {
  case (state, args) =>
    val Right(scalafmt) =
      org.scalafmt.bootstrap.ScalafmtBootstrap.fromVersion("1.3.0-16-49815ab4")
    scalafmt.main(
      List(
        "--config",
        "project/scalafmt.conf",
        "--git",
        "true",
        "--exclude",
        "deriving-macro/src/test/resources",
        "--non-interactive"
      ) ++: args
    )
    state
}
