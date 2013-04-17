name := "er_sketches"

version := "0.1.0-SNAPSHOT"

organization := "at.iem.point"

scalaVersion := "2.10.1"

homepage := Some(url("https://github.com/iem-projects/PointLib"))

licenses := Seq("GPL v2+" -> url("http://www.gnu.org/licenses/gpl-2.0.txt"))

libraryDependencies ++= Seq(
  "de.sciss" %% "scalacollider"            % "1.6.+",
//"de.sciss" %% "scalacolliderswing"       % "1.5.+",
//"de.sciss" %% "scalacolliderugens-core"  % "1.5.1+",
  "de.sciss" %% "sonogramoverview"         % "1.5.+",                       // Sonogram View
  "de.sciss" %% "strugatzki"               % "1.8.+",                       // Offline Feature Extraction
  "de.sciss" %% "audiowidgets-swing"       % "1.2.+",                       // Axis component
  "de.sciss" %% "desktop"                  % "0.2.+",                       // Menu components
  "com.github.benhutchison" % "scalaswingcontrib" % "1.5",                  // GroupPanel component for Scala-Swing
  "org.spire-math" %% "spire" % "0.3.0",                                    // Rational Numbers
  "com.github.wookietreiber" %% "scala-chart" % "0.2.0" % "test"            // JFreeChart integration
)

retrieveManaged := true

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature")

// ---- console ----

initialCommands in console := 
"""import at.iem.point.er.sketches._
  |import de.sciss.synth._
  |import ugen._
  |import Ops._
  |import spire.math._
""".stripMargin
