name := "sh_sketches"

version := "0.0.1-SNAPSHOT"

organization := "at.iem.point"

scalaVersion := "2.10.1"

homepage := Some(url("https://github.com/iem-projects/PointLib"))

licenses := Seq("GPL v2+" -> url("http://www.gnu.org/licenses/gpl-2.0.txt"))

libraryDependencies ++= Seq(
  "org.spire-math" %% "spire" % "0.4.0-M4",                        // Rational Numbers
  "com.github.wookietreiber" %% "scala-chart" % "0.2.0"            // JFreeChart integration
)

retrieveManaged := true

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature")

// ---- console ----

initialCommands in console := 
"""import at.iem.point.sh.sketches._
  |import Cell._
  |import Ladma._
  |import spire.math._
""".stripMargin

// ---- app bundle ----

// seq(appbundle.settings: _*)

// appbundle.target <<= baseDirectory

