name         := "eh-sketches"

version      := "0.2.0-SNAPSHOT"

organization := "at.iem.point"

scalaVersion := "2.10.3"

homepage     := Some(url("https://github.com/iem-projects/PointLib/"))

licenses     := Seq("GPL v3+" -> url("http://www.gnu.org/licenses/gpl-3.0.txt"))

libraryDependencies in ThisBuild ++= Seq(
  "de.sciss" %% "contextsnake"       % "0.1.1+",
  "de.sciss" %% "pointillism-core"   % "0.2.+",
  "de.sciss" %% "pointillism-views"  % "0.2.+",
  "de.sciss" %% "audiowidgets-app"   % "1.3.1+",
  "de.sciss" %% "fileutil"           % "1.1.+",
  "de.sciss" %% "numbers"            % "0.1.+",
  "de.sciss" %% "kollflitz"          % "0.1.+",
  "com.github.wookietreiber" %% "scala-chart" % "0.3.0",
  "de.sciss" %% "pdflitz"            % "1.0.1+"
)

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature")

retrieveManaged := true

initialCommands in console :=
  """import at.iem.point.eh.sketches._
    |import at.iem.point.illism._
    |import scalax.chart._
    |import scalax.chart.Charting._
    |import de.sciss.midi
    |""".stripMargin
