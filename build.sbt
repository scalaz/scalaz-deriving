val scalazVersion     = "7.2.20"
val shapelessVersion  = "2.3.3"
val simulacrumVersion = "0.12.0"

addCommandAlias("cpl", "all compile test:compile")
addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")
addCommandAlias(
  "check",
  "all headerCheck test:headerCheck scalafmtSbtCheck scalafmtCheck test:scalafmtCheck"
)
addCommandAlias("lint", "all compile:scalafixCli test:scalafixCli")

val plugin = (project in file("deriving-plugin")).settings(
  name := "deriving-plugin",
  scalafixCli in Compile := {}, // scala-compiler code quality is too low
  scalacOptions in Test += "-Yno-predef",
  scalacOptions in Test += "-Yno-imports", // checks for relative vs full fqn
  libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided",
    "org.ensime"     %% "pcplod"        % "1.2.1"            % "test",
  ),
  scalacOptions in Test ++= {
    val jar = (packageBin in Compile).value
    Seq(s"-Xplugin:${jar.getAbsolutePath}", s"-Jdummy=${jar.lastModified}")
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
    MacroParadise,
    scalacOptions += "-Yno-predef",
    scalacOptions in Test += "-Yno-imports", // checks for relative vs full fqn
    libraryDependencies ++= Seq(
      "org.scala-lang"       % "scala-compiler" % scalaVersion.value % "provided",
      "org.scala-lang"       % "scala-reflect"  % scalaVersion.value % "provided",
      "org.scalaz"           %% "scalaz-core"   % scalazVersion      % "test",
      "com.chuusai"          %% "shapeless"     % shapelessVersion   % "test",
      "org.ensime"           %% "pcplod"        % "1.2.1"            % "test",
      "com.github.mpilquist" %% "simulacrum"    % simulacrumVersion  % "test",
      "com.typesafe.play"    %% "play-json"     % "2.6.9"            % "test"
    )
  )

// extensions to scalaz7.2
val scalaz = (project in file("scalaz-deriving-base")).settings(
  KindProjector,
  name := "scalaz-deriving-base",
  licenses := Seq(
    ("BSD-3" -> url("https://opensource.org/licenses/BSD-3-Clause"))
  ),
  scalacOptions += "-Yno-imports",
  scalacOptions += "-Yno-predef",
  libraryDependencies ++= Seq(
    "com.chuusai" %% "shapeless"   % shapelessVersion,
    "org.scalaz"  %% "scalaz-core" % scalazVersion
  )
)

val derivez = (project in file("scalaz-deriving"))
  .enablePlugins(NeoJmhPlugin)
  .dependsOn(
    scalaz,
    macros % "test"
  )
  .settings(inConfig(Test)(ScalazDeriving))
  .settings(
    KindProjector,
    MacroParadise,
    name := "scalaz-deriving",
    scalacOptions += "-Yno-imports",
    scalacOptions += "-Yno-predef",
    envVars in Jmh += ("RANDOM_DATA_GENERATOR_SEED" -> "0"),
    // WORKAROUND https://gitlab.com/fommil/sbt-sensible/issues/18
    dependencyClasspathAsJars in NeoJmhPlugin.JmhInternal ++= (fullClasspathAsJars in Jmh).value,
    libraryDependencies ++= Seq(
      "org.scala-lang"      % "scala-compiler"         % scalaVersion.value % "provided",
      "io.frees"            %% "iotaz-core"            % "0.3.6",
      "com.danielasfregola" %% "random-data-generator" % "2.4" % "test,jmh"
    )
  )

val xmlformat = (project in file("examples/xmlformat"))
  .dependsOn(macros)
  .settings(inConfig(Test)(ScalazDeriving))
  .settings(
    MacroParadise,
    libraryDependencies ++= Seq(
      "org.scalaz"             %% "scalaz-core" % scalazVersion,
      "com.chuusai"            %% "shapeless"   % shapelessVersion,
      "com.github.mpilquist"   %% "simulacrum"  % simulacrumVersion,
      "org.scala-lang.modules" %% "scala-xml"   % "1.1.0"
    )
  )

// root project
skip in publish := true
