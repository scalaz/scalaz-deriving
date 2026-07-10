scalacOptions ++= Seq("-unchecked", "-deprecation")

addSbtPlugin("com.github.sbt" % "sbt-pgp" % "2.3.1")

addSbtPlugin("com.github.sbt" % "sbt-release" % "1.5.0")

addSbtPlugin("com.github.sbt" % "sbt-header" % "5.11.0")

addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.8")

addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.6.1")

addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.14.7")
