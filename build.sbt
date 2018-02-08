val scalazVersion    = "7.2.18"
val shapelessVersion = "2.3.3"

val deriving = (project in file("deriving-macro")).settings(
  name := "deriving-macro",
  MacroParadise,
  libraryDependencies ++= Seq(
    "org.scala-lang"       % "scala-compiler" % scalaVersion.value % "provided",
    "org.scala-lang"       % "scala-reflect"  % scalaVersion.value % "provided",
    "org.scalaz"           %% "scalaz-core"   % scalazVersion      % "test",
    "com.chuusai"          %% "shapeless"     % shapelessVersion   % "test",
    "org.ensime"           %% "pcplod"        % "1.2.1"            % "test",
    "com.github.mpilquist" %% "simulacrum"    % "0.11.0"           % "test",
    "com.typesafe.play"    %% "play-json"     % "2.6.8"            % "test"
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
    "com.chuusai" %% "shapeless"   % shapelessVersion,
    "org.scalaz"  %% "scalaz-core" % scalazVersion
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
      "io.frees"            %% "iotaz-core"            % "0.3.3",
      "com.danielasfregola" %% "random-data-generator" % "2.3" % "test,jmh"
    )
  )

val xmlformat = (project in file("examples/xmlformat"))
  .dependsOn(deriving)
  .settings(
    MacroParadise,
    scalacOptions -= "-Yno-imports",
    scalacOptions -= "-Yno-predef",
    libraryDependencies ++= Seq(
      "org.scalaz"             %% "scalaz-core" % scalazVersion,
      "com.chuusai"            %% "shapeless"   % shapelessVersion,
      "com.github.mpilquist"   %% "simulacrum"  % "0.11.0",
      "org.scala-lang.modules" %% "scala-xml"   % "1.0.6"
    )
  )

// root project
publishLocal := {}
publish := {}
