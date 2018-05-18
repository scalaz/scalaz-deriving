scalacOptions ++= Seq("-unchecked", "-deprecation")
ivyLoggingLevel := UpdateLogging.Quiet

addSbtPlugin("com.fommil"    % "sbt-sensible" % "2.4.5")
addSbtPlugin("com.fommil"    % "sbt-neojmh"   % "1.0.3")
addSbtPlugin("com.geirsson"  % "sbt-scalafmt" % "1.5.1")
addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.6.0-M5")
