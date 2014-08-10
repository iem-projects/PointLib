import AssemblyKeys._

name         := "sh_sketches"

version      := "0.1.0-SNAPSHOT"

organization := "at.iem.point"

scalaVersion := "2.11.2"

homepage     := Some(url("https://github.com/iem-projects/PointLib"))

licenses     := Seq("GPL v3+" -> url("http://www.gnu.org/licenses/gpl-3.0.txt"))

// required for play-json-sealed
resolvers += "Typesafe Releases" at "https://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
  "de.sciss"                 %% "muta"               % "0.4.1",   // GA framework
  "org.spire-math"           %% "spire"              % "0.7.5",   // Rational Numbers
  "com.github.wookietreiber" %% "scala-chart"        % "0.4.2",   // JFreeChart integration
  "de.sciss"                 %% "pdflitz"            % "1.1.0",   // PDF export
  "de.sciss"                 %% "pointillism-rhythm" % "0.3.0",   // Manipulating rhythmic cells
  "de.sciss"                 %% "numbers"            % "0.1.1",
  "de.sciss"                 %  "weblaf"             % "1.28"
)

// retrieveManaged := true

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-Xfuture")

// ---- console ----

initialCommands in console := 
"""import at.iem.point.sh.sketches._
  |import spire.math._
  |import spire.syntax._
""".stripMargin

// ---- app bundle ----

seq(appbundle.settings: _*)

seq(assemblySettings: _*)

appbundle.mainClass := Some("at.iem.point.sh.sketches.gui.GeneticApp")

appbundle.name      := "SH_GeneticAlgorithm"

appbundle.target    := baseDirectory.value

appbundle.icon      := Some(file("icon.png"))

test in assembly      := ()

mainClass in assembly := Some("at.iem.point.sh.sketches.gui.GeneticApp")

target  in assembly   := baseDirectory.value

jarName in assembly   := s"SH_GeneticAlgorithm-${version.value}.jar"

