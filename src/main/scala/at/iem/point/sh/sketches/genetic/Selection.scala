package at.iem.point.sh.sketches
package genetic

import de.sciss.muta
import de.sciss.muta.{SelectionPercent, SelectionSize}
import de.sciss.play.json.AutoFormat
import play.api.libs.json.Format

sealed trait SelectionImpl extends muta.Selection[GeneticSystem.Chromosome]

case class SelectionRoulette(size: SelectionSize = SelectionPercent(33))
  extends muta.impl.SelectionRouletteImpl[GeneticSystem.Chromosome] with SelectionImpl

case class SelectionTruncation(size: SelectionSize = SelectionPercent(20))
  extends muta.impl.SelectionTruncationImpl[GeneticSystem.Chromosome] with SelectionImpl

object SelectionImpl {
  implicit val format: Format[SelectionImpl] = AutoFormat[SelectionImpl]
}