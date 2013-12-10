name         := "ot-sketches"

version      := "0.3.0"

organization := "at.iem.point"

scalaVersion := "2.10.3"

homepage     := Some(url("https://github.com/iem-projects/PointLib/"))

licenses     := Seq("GPL v3+" -> url("http://www.gnu.org/licenses/gpl-3.0.txt"))

resolvers ++= Seq(
  "Typesafe Releases" at "https://repo.typesafe.com/typesafe/releases/"
)

// cf. http://stackoverflow.com/questions/20497271/sbt-does-not-resolve-typesafe-repository/20497429#20497429
resolvers += "Typesafe Simple Repository" at "http://repo.typesafe.com/typesafe/simple/maven-releases/"

libraryDependencies in ThisBuild ++= Seq(
  "de.sciss" %% "pointillism"         % "0.2.+",
  "de.sciss" %% "pdflitz"             % "1.0.+",
  "de.sciss" %% "muta"                % "0.3.2+",
  "de.sciss" %% "audiowidgets-swing"  % "1.3.1+",
  "de.sciss" %% "numbers"             % "0.1.+",
  "de.sciss" %  "abc4j"               % "0.6.+",
  "de.sciss" %% "guiflitz"            % "0.1.1+",
  "de.sciss" %% "poirot"              % "0.1.+",
  "de.sciss" %% "kollflitz"           % "0.1.+"
)

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature")

retrieveManaged := true

initialCommands in console :=
  """import at.iem.point.illism._
    |import at.iem.point.ot.sketches._
    |import scalax.chart._
    |import scalax.chart.Charting._
    |implicit val random = mkRandom()
    |import de.sciss.jacop
    |import jacop.Implicits._
    |implicit val model = new jacop.Model()
    |""".stripMargin

// ---- app bundle ----

seq(appbundle.settings: _*)

appbundle.mainClass   := Some("at.iem.point.ot.sketches.GeneticApp")

appbundle.javaOptions += "-Xmx1024m"

appbundle.name        := "OT_GeneticAlgorithm"

appbundle.target      := baseDirectory.value

appbundle.icon        := Some(file("icon.png"))

