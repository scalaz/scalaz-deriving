scalacOptions ++= Seq("-unchecked", "-deprecation")
ivyLoggingLevel := UpdateLogging.Quiet

addSbtPlugin(
  ("com.fommil" % "sbt-sensible" % "2.4.6")
    .exclude("io.get-coursier", "sbt-coursier")
)

addSbtPlugin("com.fommil" % "sbt-neojmh" % "1.1.1")

addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.5.4")

addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.14.2")

addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "3.9.21")
