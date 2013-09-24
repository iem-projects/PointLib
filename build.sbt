name := "ot-sketches"

version := "0.0.1-SNAPSHOT"

organization := "at.iem.point"

scalaVersion := "2.10.2"

homepage := Some(url("https://github.com/iem-projects/PointLib/"))

licenses := Seq("GPL v3+" -> url("http://www.gnu.org/licenses/gpl-3.0.txt"))

resolvers ++= Seq(
  "Mandubian repository snapshots" at "https://github.com/mandubian/mandubian-mvn/raw/master/snapshots/",
  "Sonatype OSS snapshots"         at "https://oss.sonatype.org/content/repositories/snapshots/"
)

libraryDependencies in ThisBuild ++= Seq(
  "de.sciss" %% "pointillism" % "0.2.+",
  "de.sciss" %% "pdflitz"     % "1.0.+",
  "de.sciss" %% "muta"        % "0.2.+",
  "de.sciss" %  "abc4j"       % "0.6.+"
)

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature")

retrieveManaged := true

initialCommands in console :=
  """import at.iem.point.illism._
    |import at.iem.point.ot.sketches._
    |import scalax.chart._
    |import scalax.chart.Charting._
    |implicit val random = mkRandom()
  """.stripMargin

// ---- app bundle ----

seq(appbundle.settings: _*)

appbundle.mainClass := Some("at.iem.point.ot.sketches.GeneticApp")

appbundle.name := "GeneticAlgorithm"

appbundle.target <<= baseDirectory

appbundle.icon := Some(file("icon.png"))

