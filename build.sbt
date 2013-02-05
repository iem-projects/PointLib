name := "eh-sketches"

version := "0.0.1-SNAPSHOT"

organization := "at.iem.point"

scalaVersion := "2.10.0"

homepage := Some(url("https://github.com/iem-projects/PointLib/"))

licenses := Seq("GPL v2+" -> url("http://www.gnu.org/licenses/gpl-2.0.txt"))

libraryDependencies in ThisBuild ++= Seq(
  "de.sciss" %% "contextsnake" % "0.1.1+"
)

retrieveManaged := true

initialCommands in console := """import at.iem.point.eh.sketches.midi"""