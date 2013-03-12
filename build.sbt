name := "ms-sketches"

version := "0.0.1-SNAPSHOT"

organization := "at.iem.point"

scalaVersion := "2.10.0"

homepage := Some(url("https://github.com/iem-projects/PointLib/"))

licenses := Seq("GPL v2+" -> url("http://www.gnu.org/licenses/gpl-2.0.txt"))

libraryDependencies in ThisBuild ++= Seq(
  "de.sciss" %% "contextsnake" % "0.1.1+",
  "de.sciss" %% "fingertree" % "1.4.+",
  "com.github.wookietreiber" %% "scala-chart" % "latest.integration",
  "com.itextpdf" % "itextpdf" % "5.3.2"
)

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-language:implicitConversions")

retrieveManaged := true

initialCommands in console :=
"""import at.iem.point.eh.sketches._
  |import scalax.chart._
  |import scalax.chart.Charting._
""".stripMargin
