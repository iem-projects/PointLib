name          := "ms-sketches"

version       := "0.1.0-SNAPSHOT"

organization  := "at.iem.point"

scalaVersion  := "2.11.6"

homepage      := Some(url("https://github.com/iem-projects/PointLib/"))

licenses      := Seq("GPL v3+" -> url("http://www.gnu.org/licenses/gpl-3.0.txt"))

libraryDependencies in ThisBuild ++= Seq(
  "de.sciss" %% "contextsnake"  % "0.2.0",
  "de.sciss" %% "fingertree"    % "1.5.2",
  "de.sciss" %% "pointillism"   % "0.3.0",
  "de.sciss" %% "pdflitz"       % "1.1.0",
  "de.sciss" %% "fileutil"      % "1.1.1",
  "de.sciss" %% "kollflitz"     % "0.2.0",
  "de.sciss" %% "numbers"       % "0.1.1",
  "tw.edu.ntu.csie" % "libsvm" % "3.17"
)

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-Xfuture")

// retrieveManaged := true

initialCommands in console :=
  """import at.iem.point.ms.sketches._
    |import scalax.chart._
    |import scalax.chart.api._
    |import de.sciss.midi
    |import de.sciss.file._
    |""".stripMargin
