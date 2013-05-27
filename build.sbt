name := "sh_sketches"

version := "0.0.1-SNAPSHOT"

organization := "at.iem.point"

scalaVersion := "2.10.1"

homepage := Some(url("https://github.com/iem-projects/PointLib"))

licenses := Seq("GPL v2+" -> url("http://www.gnu.org/licenses/gpl-2.0.txt"))

libraryDependencies ++= Seq(
  "org.spire-math"           %% "spire"              % "0.4.0-M4",        // Rational Numbers; 0.4.0 adds `lcm` methods
  "com.github.wookietreiber" %% "scala-chart"        % "0.3.0-SNAPSHOT",  // JFreeChart integration
  "de.sciss"                 %% "pdflitz"            % "1.0.+",           // PDF export
  "de.sciss"                 %% "pointillism-rhythm" % "0.2.+",
  "de.sciss"                 %% "fileutil"           % "1.0.+"
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

// seq(appbundle.settings: _*)

// appbundle.target <<= baseDirectory

