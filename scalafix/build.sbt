lazy val V = _root_.scalafix.Versions
// Use a scala version supported by scalafix.
scalaVersion in ThisBuild := V.scala212

lazy val rules = project.settings(
  libraryDependencies += "ch.epfl.scala" %% "scalafix-core" % V.version
)

lazy val input = project
  .settings(
    scalafixSourceroot := sourceDirectory.in(Compile).value,
    libraryDependencies += "com.fommil" %% "deriving-macro" % "0.10.0",
    addCompilerPlugin(
      "org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full)
  )
  .disablePlugins(ScalafmtPlugin)

lazy val output = project.disablePlugins(ScalafmtPlugin)

lazy val tests = project
  .settings(
    scalafmtConfig := Some(file("../project/scalafmt.conf")),
    libraryDependencies += "ch.epfl.scala" % "scalafix-testkit" % V.version % Test cross CrossVersion.full,
    buildInfoPackage := "fix",
    buildInfoKeys := Seq[BuildInfoKey](
      "inputSourceroot" ->
        sourceDirectory.in(input, Compile).value,
      "outputSourceroot" ->
        sourceDirectory.in(output, Compile).value,
      "inputClassdirectory" ->
        classDirectory.in(input, Compile).value
    )
  )
  .dependsOn(input, rules)
  .enablePlugins(BuildInfoPlugin)
