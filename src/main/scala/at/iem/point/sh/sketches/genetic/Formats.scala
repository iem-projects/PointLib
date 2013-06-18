package at.iem.point.sh.sketches.genetic

import play.api.libs.json.SealedTraitFormat

object Formats {
  implicit val selection = SealedTraitFormat[SelectionSize]
}