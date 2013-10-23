name := "er_sketches"

version := "0.2.0-SNAPSHOT"

organization := "at.iem.point"

scalaVersion := "2.10.2"

homepage := Some(url("https://github.com/iem-projects/PointLib"))

licenses := Seq("GPL v2+" -> url("http://www.gnu.org/licenses/gpl-2.0.txt"))

// need for play-json-sealed
resolvers in ThisBuild ++= Seq(
  "Mandubian repository snapshots" at "https://github.com/mandubian/mandubian-mvn/raw/master/snapshots/",
  "Sonatype OSS snapshots"         at "https://oss.sonatype.org/content/repositories/snapshots/"
)

libraryDependencies ++= Seq(
  "de.sciss" %% "scalacollider"            % "1.8.+",
  "de.sciss" %% "sonogramoverview"         % "1.6.+",                       // Sonogram View
  "de.sciss" %% "strugatzki"               % "2.0.+",                       // Offline Feature Extraction
  "de.sciss" %% "audiowidgets-app"         % "1.3.+",                       // Timeline components
  "de.sciss" %% "fileutil"                 % "1.0.+",                       // Easy file navigation
  "de.sciss" %% "play-json-sealed"         % "0.0.+",                       // JSON serialization
  "com.github.benhutchison" % "scalaswingcontrib" % "1.5",                  // GroupPanel component for Scala-Swing
  "org.spire-math" %% "spire" % "0.3.0",                                    // Rational Numbers
  //
  "com.github.wookietreiber" %% "scala-chart" % "0.2.0" % "test",           // JFreeChart integration
  "de.sciss" %% "pdflitz" % "1.0.+" % "test",
  "de.sciss" %% "numbers" % "0.1.+" % "test"
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

appbundle.name := "ER_FeatureExtraction"

appbundle.icon := Some(file("icon.png"))

appbundle.target <<= baseDirectory

