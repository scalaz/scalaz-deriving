import sbtrelease.ReleaseStateTransformations.*

val scalazVersion     = "7.3.8"
val shapelessVersion  = "2.3.13"
val simulacrumVersion = "1.0.1"
val magnoliaVersion   = "0.12.8"
val refinedVersion    = "0.11.3"
val newtypeVersion    = "0.4.4"

addCommandAlias("cpl", "all Test/compile Jmh/compile")
addCommandAlias("fmt", "all scalafmtSbt scalafmtAll")
addCommandAlias(
  "check",
  "all headerCheck Test/headerCheck Jmh/headerCheck scalafmtSbtCheck scalafmtCheckAll"
)
addCommandAlias(
  "lint",
  s";++ $Scala212;scalafix --check;Test/scalafix --check"
)
addCommandAlias("fix", s";++ $Scala212;all compile:scalafix test:scalafix")

val plugin = (project in file("deriving-plugin")).settings(
  name := "deriving-plugin",
  Test / scalacOptions += "-Yno-predef",
  Test / scalacOptions += "-Yno-imports", // checks for relative vs full fqn
  crossScalaVersions := ProjectKeys.allScalaVersions,
  crossVersion       := CrossVersion.full,
  crossTarget        := {
    // workaround for https://github.com/sbt/sbt/issues/5097
    target.value / s"scala-${scalaVersion.value}"
  },
  libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided"
  ),
  (Test / scalacOptions) ++= {
    val jar = (Compile / packageBin).value
    Seq(s"-Xplugin:${jar.getAbsolutePath}", s"-Jdummy=${jar.lastModified}")
  }
)

// doesn't sbt offer a way to do this?
def ScalazDeriving: Seq[Setting[_]] =
  Seq(
    scalacOptions ++= {
      val jar = (plugin / Compile / packageBin).value
      Seq(s"-Xplugin:${jar.getAbsolutePath}", s"-Jdummy=${jar.lastModified}")
    }
  )

val macros = (project in file("deriving-macro"))
  .settings(ScalazDeriving)
  .settings(
    name                    := "deriving-macro",
    MacroParadise,
    exportJars              := true,
    Test / managedClasspath := {
      val res = (Test / resourceDirectory).value
      val old = (Test / managedClasspath).value
      Attributed.blank(res) +: old
    },
    scalacOptions += "-Yno-predef",
    Test / scalacOptions += "-Yno-imports", // checks for relative vs full fqn
    Test / sources          := {
      val x = (Test / sources).value
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 13)) =>
          if ((0.to(6)).map("2.13." + _).toSet.contains(scalaVersion.value)) {
            x
          } else {
            Nil // TODO enable tests
          }
        case _             =>
          x
      }
    },
    libraryDependencies ++= Seq(
      "org.scala-lang"     % "scala-compiler" % scalaVersion.value % "provided",
      "org.scala-lang"     % "scala-reflect"  % scalaVersion.value % "provided",
      "org.scalaz"        %% "scalaz-core"    % scalazVersion      % "test",
      "com.chuusai"       %% "shapeless"      % shapelessVersion   % "test",
      "org.typelevel"     %% "simulacrum"     % simulacrumVersion  % "test",
      "org.playframework" %% "play-json"      % "3.0.6"            % "test",
      "io.estatico"       %% "newtype"        % newtypeVersion     % "test"
    )
  )

// extensions to scalaz7.2
val base = (project in file("scalaz-deriving-base")).settings(
  KindProjector,
  MonadicFor,
  name          := "scalaz-deriving-base",
  licenses      := Seq(
    ("BSD-3" -> url("https://opensource.org/licenses/BSD-3-Clause"))
  ),
  headerLicense := Some(
    HeaderLicense.BSD3Clause(
      yyyy = ProjectPlugin.startYearValue.toString,
      copyrightOwner = "Sam Halliday",
      licenseStyle = HeaderLicenseStyle.SpdxSyntax
    )
  ),
  scalacOptions += "-Yno-imports",
  scalacOptions += "-Yno-predef",
  libraryDependencies ++= Seq(
    "org.scala-lang"              % "scala-reflect"             % scalaVersion.value % "provided",
    "com.github.alexarchambault" %% "scalacheck-shapeless_1.15" % "1.3.0"            % "test",
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
      "org.scalaz"     %% "scalaz-scalacheck-binding" % scalazVersion
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
      "io.estatico"   %% "newtype"        % newtypeVersion     % "test",
      "org.typelevel" %% "simulacrum"     % simulacrumVersion  % "test",
      "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided"
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
      "com.fasterxml.woodstox" % "woodstox-core" % "7.1.1",
      "eu.timepit"            %% "refined"       % refinedVersion,
      "org.scalaz"            %% "scalaz-core"   % scalazVersion,
      "com.chuusai"           %% "shapeless"     % shapelessVersion,
      "org.typelevel"         %% "simulacrum"    % simulacrumVersion
    )
  )
  .enablePlugins(NeoJmhPlugin)
  .settings(headerSettings(Jmh))
  .settings(
    org.scalafmt.sbt.ScalafmtPlugin.scalafmtConfigSettings(Jmh)
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
      "eu.timepit"        %% "refined"         % refinedVersion,
      "org.scalaz"        %% "scalaz-core"     % scalazVersion,
      "org.typelevel"     %% "simulacrum"      % simulacrumVersion,
      "org.scalatestplus" %% "scalacheck-1-17" % "3.2.18.0",
      "org.typelevel"     %% "jawn-parser"     % "1.6.0"
    )
    // addCompilerPlugin("ch.epfl.scala" %% "scalac-profiling" % "1.0.0"),
    // scalacOptions ++= Seq("-Ystatistics:typer", "-P:scalac-profiling:no-profiledb")
  )
  .enablePlugins(NeoJmhPlugin)
  .settings(headerSettings(Jmh))
  .settings(
    org.scalafmt.sbt.ScalafmtPlugin.scalafmtConfigSettings(Jmh)
  )

// root project
publish / skip := true

releaseCrossBuild := true

releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runTest,
  setReleaseVersion,
  commitReleaseVersion,
  tagRelease,
  releaseStepCommandAndRemaining("+publishSigned"),
  releaseStepCommandAndRemaining("sonaRelease"),
  setNextVersion,
  commitNextVersion,
  pushChanges
)
