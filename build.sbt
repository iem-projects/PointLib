import AssemblyKeys._

name         := "er_sketches"

version      := "0.3.0-SNAPSHOT"

organization := "at.iem.point"

scalaVersion := "2.11.2" // "2.10.3"

homepage     := Some(url("https://github.com/iem-projects/PointLib"))

licenses     := Seq("GPL v2+" -> url("http://www.gnu.org/licenses/gpl-2.0.txt"))

// need for play-json-sealed
// resolvers in ThisBuild ++= Seq(
//   "Mandubian repository snapshots" at "https://github.com/mandubian/mandubian-mvn/raw/master/snapshots/",
//   "Sonatype OSS snapshots"         at "https://oss.sonatype.org/content/repositories/snapshots/"
// )

resolvers += "Typesafe Releases" at "https://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
  "de.sciss" %% "scalacollider"            % "1.12.0",
  "de.sciss" %% "sonogramoverview"         % "1.7.1",                       // Sonogram View
  "de.sciss" %% "strugatzki"               % "2.4.1",                       // Offline Feature Extraction
  "de.sciss" %% "audiowidgets-app"         % "1.6.2",                       // Timeline components
  "de.sciss" %% "fileutil"                 % "1.1.1",                       // Easy file navigation
  "de.sciss" %% "play-json-sealed"         % "0.2.0",                       // JSON serialization
  "de.sciss" %% "swingplus"                % "0.1.2",
  "de.sciss" %  "weblaf"                   % "1.28",
  "org.spire-math" %% "spire" % "0.8.2",                                    // Rational Numbers
  "com.github.wookietreiber" %% "scala-chart" % "0.4.2" % "test",           // JFreeChart integration
  "de.sciss" %% "pdflitz" % "1.1.0" % "test",
  "de.sciss" %% "numbers" % "0.1.1" % "test"
)

// retrieveManaged := true

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-Xfuture")

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

seq(assemblySettings: _*)

appbundle.name   := "ER_FeatureExtraction"

appbundle.icon   := Some(file("icon.png"))

appbundle.target := baseDirectory.value

test in assembly      := ()

// mainClass in assembly := Some("at.iem.point.ot.sketches.GeneticApp")

target  in assembly   := baseDirectory.value

jarName in assembly   := s"ER_FeatureExtraction-${version.value}.jar"

