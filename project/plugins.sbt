scalacOptions ++= Seq("-unchecked", "-deprecation")
ivyLoggingLevel := UpdateLogging.Quiet

addSbtPlugin("com.fommil"    % "sbt-sensible" % "2.4.6")
addSbtPlugin("com.fommil"    % "sbt-neojmh"   % "1.1.1")
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.0.2")
addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.6.0-M14-3")
