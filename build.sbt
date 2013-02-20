name := "er-sketches"

version := "0.0.1-SNAPSHOT"

organization := "at.iem.point"

scalaVersion := "2.10.0"

homepage := Some(url("https://github.com/iem-projects/PointLib"))

licenses := Seq("GPL v2+" -> url("http://www.gnu.org/licenses/gpl-2.0.txt"))

libraryDependencies ++= Seq(
  "de.sciss" %% "scalacolliderswing" % "1.4.+",
  "de.sciss" %% "sonogramoverview" % "1.4.+",
  "de.sciss" %% "strugatzki" % "1.5.+",
  "com.github.benhutchison" % "scalaswingcontrib" % "1.4"
)

retrieveManaged := true

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature")

// ---- console ----

initialCommands in console := 
"""import at.iem.point.er.sketches._
  |import de.sciss.synth._
  |import ugen._
  |import Ops._
""".stripMargin
