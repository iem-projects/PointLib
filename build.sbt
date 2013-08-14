name := "eh-sketches"

version := "0.1.0-SNAPSHOT"

organization := "at.iem.point"

scalaVersion := "2.10.2"

homepage := Some(url("https://github.com/iem-projects/PointLib/"))

licenses := Seq("GPL v2+" -> url("http://www.gnu.org/licenses/gpl-2.0.txt"))

libraryDependencies in ThisBuild ++= Seq(
  "de.sciss" %% "contextsnake"       % "0.1.1+",
  "de.sciss" %% "pointillism-core"   % "0.2.+",
  "de.sciss" %% "pointillism-views"  % "0.2.+",
  "de.sciss" %% "audiowidgets-app"   % "1.3.+",
  "de.sciss" %% "fileutil"           % "1.0.+",
  "com.github.wookietreiber" %% "scala-chart" % "latest.integration",
  "de.sciss" %% "pdflitz"            % "1.0.+"
)

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-language:implicitConversions")

retrieveManaged := true

initialCommands in console :=
  """import at.iem.point.eh.sketches._
    |import at.iem.point.illism._
    |import scalax.chart._
    |import scalax.chart.Charting._
    |import de.sciss.midi
    |""".stripMargin
