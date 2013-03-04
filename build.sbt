name := "er_sketches"

version := "0.0.1-SNAPSHOT"

organization := "at.iem.point"

scalaVersion := "2.10.0"

homepage := Some(url("https://github.com/iem-projects/PointLib"))

licenses := Seq("GPL v2+" -> url("http://www.gnu.org/licenses/gpl-2.0.txt"))

libraryDependencies ++= Seq(
  "de.sciss" %% "scalacollider" % "1.5.2+",
  "de.sciss" %% "scalacolliderswing" % "1.5.+",
  "de.sciss" %% "scalacolliderugens-core" % "1.5.1+",
  "de.sciss" %% "sonogramoverview" % "1.4.+",                               // Sonogram View
  "de.sciss" %% "strugatzki" % "1.6.+",                                     // Offline Feature Extraction
  "com.github.benhutchison" % "scalaswingcontrib" % "1.4",                  // Tree component for Scala-Swing
  "com.github.wookietreiber" %% "scala-chart" % "latest.integration"        // JFreeChart integration
)

retrieveManaged := true

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature")

// ---- console ----

initialCommands in console := 
"""import at.iem.point.er.sketches._
  |import de.sciss.synth._
  |import ugen._
  |import Ops._
  |import scalax.chart._
  |import Charting._
""".stripMargin
