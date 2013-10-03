name := "sh_sketches"

version := "0.0.1-SNAPSHOT"

organization := "at.iem.point"

scalaVersion := "2.10.2"

homepage := Some(url("https://github.com/iem-projects/PointLib"))

licenses := Seq("GPL v2+" -> url("http://www.gnu.org/licenses/gpl-2.0.txt"))

// required for play-json
resolvers += "Mandubian repository snapshots" at "https://github.com/mandubian/mandubian-mvn/raw/master/snapshots/"

libraryDependencies ++= Seq(
  "org.spire-math"           %% "spire"              % "0.4.0",           // Rational Numbers
  "com.github.wookietreiber" %% "scala-chart"        % "0.3.0-SNAPSHOT",  // JFreeChart integration
  "de.sciss"                 %% "pdflitz"            % "1.0.+",           // PDF export
  "de.sciss"                 %% "pointillism-rhythm" % "0.2.+",           // Manipulating rhythmic cells
  "de.sciss"                 %% "fileutil"           % "1.0.+",           // Easy file representation
  "de.sciss"                 %% "desktop"            % "0.3.+",
  "de.sciss"                 %% "treetable-scala"    % "1.3.+",
  "de.sciss"                 %% "guiflitz"           % "0.0.2+",          // Automatic configuration GUIs
  "de.sciss"                 %% "processor"          % "0.2.+",           // Asynchronous iteration
  // "play"                     %% "play-json"          % "2.2-SNAPSHOT"     // JSON
  "de.sciss"                 %% "play-json-sealed"   % "0.0.+"
)

retrieveManaged := true

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature")

// ---- console ----

initialCommands in console := 
"""import at.iem.point.sh.sketches._
  |import spire.math._
  |import spire.syntax._
""".stripMargin

// ---- app bundle ----

seq(appbundle.settings: _*)

appbundle.mainClass := Some("at.iem.point.sh.sketches.gui.GeneticApp")

appbundle.name := "SH_GeneticAlgorithm"

appbundle.target <<= baseDirectory

appbundle.icon := Some(file("icon.png"))
