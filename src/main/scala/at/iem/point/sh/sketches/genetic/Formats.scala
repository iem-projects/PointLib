package at.iem.point.sh.sketches.genetic

import play.api.libs.json.Format
import de.sciss.play.json.AutoFormat

// note: running into SI-7588 - when this happens,
// comment out the real stuff below, uncomment the ??? stuff, and then vice versa
object Formats {
//  implicit val scaleFunction        : Format[ScaleFunction]       = ???
//  implicit val matchFunction        : Format[MatchFunction]       = ???
//  implicit val percentage           : Format[Percentage]          = ???
//  implicit val selectionSize        : Format[SelectionSize]       = ???
//  implicit val selection            : Format[Selection]           = ???
//  implicit lazy val breedingFunction: Format[BreedingFunction]    = ???
//  implicit val breeding             : Format[Breeding]            = ???
//  implicit val chromosomeFunction   : Format[ChromosomeFunction]  = ???
//  implicit val aggregateFunction    : Format[AggregateFunction]   = ???
//  implicit val windowFunction       : Format[WindowFunction]      = ???
//  implicit val localFunction        : Format[LocalFunction]       = ???
//  implicit lazy val evaluation      : Format[Evaluation]          = ???
//  implicit val settings             : Format[Settings]            = ???

  implicit val scaleFunction        : Format[ScaleFunction]       = AutoFormat[ScaleFunction]
  implicit val matchFunction        : Format[MatchFunction]       = AutoFormat[MatchFunction]
  implicit val percentage           : Format[Percentage]          = AutoFormat[Percentage]

  // implicit val selectionSize        : Format[SelectionSize]       = AutoFormat[SelectionSize]
  // implicit val selection            : Format[Selection]           = AutoFormat[Selection]

  implicit lazy val breedingFunction: Format[BreedingFunction]    = AutoFormat[BreedingFunction]
  // implicit val breeding             : Format[Breeding]            = AutoFormat[Breeding]

  implicit val chromosomeFunction   : Format[ChromosomeFunction]  = AutoFormat[ChromosomeFunction]
  implicit val aggregateFunction    : Format[AggregateFunction]   = AutoFormat[AggregateFunction]
  implicit val windowFunction       : Format[WindowFunction]      = AutoFormat[WindowFunction]
  implicit val localFunction        : Format[LocalFunction]       = AutoFormat[LocalFunction]

  // wooo, I'm amazed that this works despite recursivity !?
  implicit lazy val evaluation      : Format[Evaluation]          = AutoFormat[Evaluation]

  implicit val generation           : Format[Generation]          = AutoFormat[Generation]
}