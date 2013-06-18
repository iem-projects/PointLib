package at.iem.point.sh.sketches.genetic

import play.api.libs.json.{Format, SealedTraitFormat}

object Formats {
  implicit val percentage         = SealedTraitFormat[Percentage]

  implicit val selectionSize      = SealedTraitFormat[SelectionSize]
  implicit val selection          = SealedTraitFormat[Selection]

  implicit lazy val breedingFunction: Format[BreedingFunction] = SealedTraitFormat[BreedingFunction]
  implicit val breeding           = SealedTraitFormat[Breeding]

  implicit val matchFunction      = SealedTraitFormat[MatchFunction]
  implicit val chromosomeFunction = SealedTraitFormat[ChromosomeFunction]
  implicit val aggregateFunction  = SealedTraitFormat[AggregateFunction]
  implicit val windowFunction     = SealedTraitFormat[WindowFunction]
  implicit val localFunction      = SealedTraitFormat[LocalFunction]

  // wooo, I'm amazed that this works despite recursivity !?
  implicit lazy val evaluation: Format[Evaluation] = SealedTraitFormat[Evaluation]

  implicit val settings           = SealedTraitFormat[Settings]
}