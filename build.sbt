name         := "eh-sketches"

version      := "0.2.0-SNAPSHOT"

organization := "at.iem.point"

scalaVersion := "2.10.4"

homepage     := Some(url("https://github.com/iem-projects/PointLib/"))

licenses     := Seq("GPL v3+" -> url("http://www.gnu.org/licenses/gpl-3.0.txt"))

libraryDependencies in ThisBuild ++= Seq(
  "de.sciss" %% "contextsnake"       % "0.1.1+",
  "de.sciss" %% "pointillism-core"   % "0.2.+",
  "de.sciss" %% "pointillism-views"  % "0.2.+",
  "de.sciss" %% "audiowidgets-app"   % "1.6.+",
  "de.sciss" %% "fileutil"           % "1.1.+",
  "de.sciss" %% "numbers"            % "0.1.+",
  "de.sciss" %% "kollflitz"          % "0.1.+",
  "com.github.wookietreiber" %% "scala-chart" % "0.3.0", // XXX TODO: v0.4.0 - API changed
  "de.sciss" %% "pdflitz"            % "1.0.1+",
  // ---- fuzzy schnuck ----
  "de.sciss" %% "lucredata-core"     % "2.2.2+"
)

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-Xfuture")

// retrieveManaged := true

initialCommands in console :=
  """import at.iem.point.eh.sketches._
    |import at.iem.point.illism._
    |import scalax.chart._
    |import scalax.chart.Charting._
    |import de.sciss.midi
    |""".stripMargin
