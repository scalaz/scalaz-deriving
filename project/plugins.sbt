scalacOptions ++= Seq("-unchecked", "-deprecation")
ivyLoggingLevel := UpdateLogging.Quiet

addSbtPlugin("com.fommil" % "sbt-sensible" % "2.4.6")

addSbtPlugin("com.fommil" % "sbt-neojmh" % "1.1.1")

addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.4.4")

addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.9.33")

addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "3.9.10")
