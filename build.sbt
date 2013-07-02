name := "er_sketches"

version := "0.2.0-SNAPSHOT"

organization := "at.iem.point"

scalaVersion := "2.10.2"

homepage := Some(url("https://github.com/iem-projects/PointLib"))

licenses := Seq("GPL v2+" -> url("http://www.gnu.org/licenses/gpl-2.0.txt"))

libraryDependencies ++= Seq(
  "de.sciss" %% "scalacollider"            % "1.8.+",
  "de.sciss" %% "sonogramoverview"         % "1.6.+",                       // Sonogram View
  "de.sciss" %% "strugatzki"               % "2.0.+",                       // Offline Feature Extraction
  "de.sciss" %% "audiowidgets-swing"       % "1.2.+",                       // Axis component
  "de.sciss" %% "desktop"                  % "0.3.+",                       // Menu components
  "de.sciss" %% "fileutil"                 % "1.0.+",                       // Easy file navigation
  "de.sciss" %% "swingplus"                % "0.0.+",
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

// ---- app bundle ----

seq(appbundle.settings: _*)

appbundle.name := "FeatureExtraction"

appbundle.icon := Some(file("icon.png"))

appbundle.target <<= baseDirectory

