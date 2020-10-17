val scalazVersion     = "7.2.30"
val shapelessVersion  = "2.3.3"
val simulacrumVersion = "0.19.0"
val magnoliaVersion   = "0.12.8"
val refinedVersion    = "0.9.17"
val newtypeVersion    = "0.4.4"

addCommandAlias("cpl", "all compile test:compile jmh:compile")
addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt jmh:scalafmt")
addCommandAlias(
  "check",
  "all headerCheck test:headerCheck jmh:headerCheck scalafmtSbtCheck scalafmtCheck test:scalafmtCheck jmh:scalafmtCheck"
)
addCommandAlias(
  "lint",
  ";++ 2.12.11;compile:scalafix --check;test:scalafix --check"
)
addCommandAlias("fix", ";++ 2.12.11;all compile:scalafix test:scalafix")

val plugin = (project in file("deriving-plugin")).settings(
  name := "deriving-plugin",
  scalacOptions in Test += "-Yno-predef",
  scalacOptions in Test += "-Yno-imports", // checks for relative vs full fqn
  crossScalaVersions := Seq(
    "2.12.8",
    "2.12.9",
    "2.12.10",
    "2.12.11",
    "2.13.0",
    "2.13.1",
    "2.13.2"
  ),
  crossVersion := CrossVersion.full,
  crossTarget := {
    // workaround for https://github.com/sbt/sbt/issues/5097
    target.value / s"scala-${scalaVersion.value}"
  },
  libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided"
  ),
  scalacOptions in Test ++= {
    val jar = (packageBin in Compile).value
    Seq(s"-Xplugin:${jar.getAbsolutePath}", s"-Jdummy=${jar.lastModified}")
  }
)

// doesn't sbt offer a way to do this?
def ScalazDeriving: Seq[Setting[_]] =
  Seq(
    scalacOptions ++= {
      val jar = (packageBin in (plugin, Compile)).value
      Seq(s"-Xplugin:${jar.getAbsolutePath}", s"-Jdummy=${jar.lastModified}")
    }
  )

val macros = (project in file("deriving-macro"))
  .settings(ScalazDeriving)
  .settings(
    name := "deriving-macro",
    MacroParadise,
    resourcesOnCompilerCp(Test),
    scalacOptions += "-Yno-predef",
    scalacOptions in Test += "-Yno-imports", // checks for relative vs full fqn
    libraryDependencies ++= Seq(
      "org.scala-lang"        % "scala-compiler" % scalaVersion.value % "provided",
      "org.scala-lang"        % "scala-reflect"  % scalaVersion.value % "provided",
      "org.scalaz"           %% "scalaz-core"    % scalazVersion      % "test",
      "com.chuusai"          %% "shapeless"      % shapelessVersion   % "test",
      "com.github.mpilquist" %% "simulacrum"     % simulacrumVersion  % "test",
      "com.typesafe.play"    %% "play-json"      % "2.8.1"            % "test",
      "io.estatico"          %% "newtype"        % newtypeVersion     % "test"
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
    "org.scala-lang"              % "scala-reflect"             % scalaVersion.value % "provided",
    "com.github.alexarchambault" %% "scalacheck-shapeless_1.14" % "1.2.5"            % "test",
    "org.scalaz"                 %% "scalaz-core"               % scalazVersion
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
      "io.estatico"          %% "newtype"        % newtypeVersion     % "test",
      "com.github.mpilquist" %% "simulacrum"     % simulacrumVersion  % "test",
      "org.scala-lang"        % "scala-compiler" % scalaVersion.value % "provided"
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
      "com.fasterxml.woodstox" % "woodstox-core" % "6.2.1",
      "eu.timepit"            %% "refined"       % refinedVersion,
      "org.scalaz"            %% "scalaz-core"   % scalazVersion,
      "com.chuusai"           %% "shapeless"     % shapelessVersion,
      "com.github.mpilquist"  %% "simulacrum"    % simulacrumVersion
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
      "eu.timepit"           %% "refined"         % refinedVersion,
      "org.scalaz"           %% "scalaz-core"     % scalazVersion,
      "com.github.mpilquist" %% "simulacrum"      % simulacrumVersion,
      "org.scalatestplus"    %% "scalacheck-1-14" % "3.1.2.0",
      "org.typelevel"        %% "jawn-parser"     % "1.0.0"
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
