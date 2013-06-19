package at.iem.point.sh.sketches.genetic

import play.api.libs.json.{Format, SealedTraitFormat}

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

  implicit val scaleFunction        : Format[ScaleFunction]       = SealedTraitFormat[ScaleFunction]
  implicit val matchFunction        : Format[MatchFunction]       = SealedTraitFormat[MatchFunction]
  implicit val percentage           : Format[Percentage]          = SealedTraitFormat[Percentage]

  implicit val selectionSize        : Format[SelectionSize]       = SealedTraitFormat[SelectionSize]
  implicit val selection            : Format[Selection]           = SealedTraitFormat[Selection]

  implicit lazy val breedingFunction: Format[BreedingFunction]    = SealedTraitFormat[BreedingFunction]
  implicit val breeding             : Format[Breeding]            = SealedTraitFormat[Breeding]

  implicit val chromosomeFunction   : Format[ChromosomeFunction]  = SealedTraitFormat[ChromosomeFunction]
  implicit val aggregateFunction    : Format[AggregateFunction]   = SealedTraitFormat[AggregateFunction]
  implicit val windowFunction       : Format[WindowFunction]      = SealedTraitFormat[WindowFunction]
  implicit val localFunction        : Format[LocalFunction]       = SealedTraitFormat[LocalFunction]

  // wooo, I'm amazed that this works despite recursivity !?
  implicit lazy val evaluation      : Format[Evaluation]          = SealedTraitFormat[Evaluation]

  implicit val settings             : Format[Settings]            = SealedTraitFormat[Settings]
}