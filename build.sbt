val scalazVersion     = "7.2.28"
val shapelessVersion  = "2.3.3"
val simulacrumVersion = "0.19.0"
val magnoliaVersion   = "0.10.0"

addCommandAlias("cpl", "all compile test:compile jmh:compile")
addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt jmh:scalafmt")
addCommandAlias(
  "check",
  "all headerCheck test:headerCheck jmh:headerCheck scalafmtSbtCheck scalafmtCheck test:scalafmtCheck jmh:scalafmtCheck"
)
addCommandAlias("lint", "all compile:scalafixTest test:scalafixTest")
addCommandAlias("fix", "all compile:scalafixCli test:scalafixCli")

val plugin = (project in file("deriving-plugin")).settings(
  name := "deriving-plugin",
  scalafixCli in Compile := {}, // scala-compiler code quality is too low
  scalafixTest in Compile := {},
  scalacOptions in Test += "-Yno-predef",
  scalacOptions in Test += "-Yno-imports", // checks for relative vs full fqn
  libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided",
    "org.ensime"     %% "pcplod"        % "1.2.1"            % "test"
  ),
  scalacOptions in Test ++= {
    val jar = (packageBin in Compile).value
    Seq(s"-Xplugin:${jar.getAbsolutePath}", s"-Jdummy=${jar.lastModified}")
  },
  javaOptions in Test += {
    val jar = (packageBin in Compile).value.getAbsolutePath
    s"-Dpcplod.settings=-Ymacro-expand:discard,-Xplugin:$jar",
  }
)

// doesn't sbt offer a way to do this?
def ScalazDeriving: Seq[Setting[_]] = Seq(
  scalacOptions ++= {
    val jar = (packageBin in (plugin, Compile)).value
    Seq(s"-Xplugin:${jar.getAbsolutePath}", s"-Jdummy=${jar.lastModified}")
  }
)

val macros = (project in file("deriving-macro"))
  .settings(ScalazDeriving)
  .settings(
    name := "deriving-macro",
    scalafixCli in Compile := {}, // scala-compiler code quality is too low
    scalafixTest in Compile := {},
    MacroParadise,
    resourcesOnCompilerCp(Test),
    scalacOptions += "-Yno-predef",
    scalacOptions in Test += "-Yno-imports", // checks for relative vs full fqn
    libraryDependencies ++= Seq(
      "org.scala-lang"       % "scala-compiler" % scalaVersion.value % "provided",
      "org.scala-lang"       % "scala-reflect"  % scalaVersion.value % "provided",
      "org.scalaz"           %% "scalaz-core"   % scalazVersion      % "test",
      "com.chuusai"          %% "shapeless"     % shapelessVersion   % "test",
      "com.github.mpilquist" %% "simulacrum"    % simulacrumVersion  % "test",
      "com.typesafe.play"    %% "play-json"     % "2.7.4"            % "test",
      "io.estatico"          %% "newtype"       % "0.4.3"            % "test"
    )
  )

// extensions to scalaz7.2
val base = (project in file("scalaz-deriving-base")).settings(
  KindProjector,
  MonadicFor,
  name := "scalaz-deriving-base",
  licenses := Seq(
    ("BSD-3" -> url("https://opensource.org/licenses/BSD-3-Clause"))
  ),
  scalacOptions += "-Yno-imports",
  scalacOptions += "-Yno-predef",
  libraryDependencies ++= Seq(
    "org.scalaz" %% "scalaz-core" % scalazVersion
  )
)

val magnolia = project
  .dependsOn(
    macros % "test"
  )
  .settings(ScalazDeriving)
  .settings(
    name := "scalaz-deriving-magnolia",
    libraryDependencies ++= Seq(
      "com.propensive" %% "magnolia"    % magnoliaVersion,
      "org.scalaz"     %% "scalaz-core" % scalazVersion
    )
  )

val shapeless = project
  .dependsOn(
    macros % "test"
  )
  .settings(ScalazDeriving)
  .settings(
    name := "scalaz-deriving-shapeless",
    libraryDependencies ++= Seq(
      "com.chuusai" %% "shapeless"   % shapelessVersion,
      "org.scalaz"  %% "scalaz-core" % scalazVersion
    )
  )

val scalacheck = project
  .dependsOn(
    macros % "test"
  )
  .settings(ScalazDeriving)
  .settings(
    name := "scalaz-deriving-scalacheck",
    libraryDependencies ++= Seq(
      "com.propensive" %% "magnolia"                  % magnoliaVersion,
      "org.scalaz"     %% "scalaz-scalacheck-binding" % s"$scalazVersion-scalacheck-1.14"
    )
  )

val deriving = (project in file("scalaz-deriving"))
  .dependsOn(
    base,
    macros,
    magnolia   % "test",
    scalacheck % "test"
  )
  .settings(inConfig(Test)(ScalazDeriving))
  .settings(
    KindProjector,
    MacroParadise,
    MonadicFor,
    name := "scalaz-deriving",
    scalacOptions += "-Yno-imports",
    scalacOptions += "-Yno-predef",
    libraryDependencies ++= Seq(
      "io.estatico"          %% "newtype"       % "0.4.3" % "test",
      "com.github.mpilquist" %% "simulacrum"    % simulacrumVersion % "test",
      "org.scala-lang"       % "scala-compiler" % scalaVersion.value % "provided",
      "io.frees"             %% "iotaz-core"    % "0.3.10"
    )
  )

val xmlformat = (project in file("examples/xmlformat"))
  .dependsOn(deriving, magnolia, scalacheck)
  .settings(ScalazDeriving)
  .settings(
    name := "scalaz-deriving-xmlformat",
    KindProjector,
    MacroParadise,
    MonadicFor,
    libraryDependencies ++= Seq(
      "com.fasterxml.woodstox" % "woodstox-core" % "5.3.0",
      "eu.timepit"             %% "refined"      % "0.9.9",
      "org.scalaz"             %% "scalaz-core"  % scalazVersion,
      "com.chuusai"            %% "shapeless"    % shapelessVersion,
      "com.github.mpilquist"   %% "simulacrum"   % simulacrumVersion
    )
  )
  .enablePlugins(NeoJmhPlugin)
  .settings(headerSettings(Jmh))
  .settings(
    inConfig(Jmh)(org.scalafmt.sbt.ScalafmtPlugin.scalafmtConfigSettings)
  )

val jsonformat = (project in file("examples/jsonformat"))
  .dependsOn(deriving, magnolia, scalacheck, shapeless % "test")
  .settings(ScalazDeriving)
  .settings(
    name := "scalaz-deriving-jsonformat",
    KindProjector,
    MacroParadise,
    MonadicFor,
    libraryDependencies ++= Seq(
      "eu.timepit"           %% "refined"     % "0.9.9",
      "org.scalaz"           %% "scalaz-core" % scalazVersion,
      "com.github.mpilquist" %% "simulacrum"  % simulacrumVersion,
      "org.typelevel"        %% "jawn-parser" % "0.14.2"
    )
    //addCompilerPlugin("ch.epfl.scala" %% "scalac-profiling" % "1.0.0"),
    //scalacOptions ++= Seq("-Ystatistics:typer", "-P:scalac-profiling:no-profiledb")
  )
  .enablePlugins(NeoJmhPlugin)
  .settings(headerSettings(Jmh))
  .settings(
    inConfig(Jmh)(
      org.scalafmt.sbt.ScalafmtPlugin.scalafmtConfigSettings
    )
  )

// root project
skip in publish := true
