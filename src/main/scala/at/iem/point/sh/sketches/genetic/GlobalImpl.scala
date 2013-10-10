package at.iem.point.sh.sketches
package genetic

import de.sciss.play.json.AutoFormat
import play.api.libs.json.Format
import at.iem.point.illism.rhythm.Cell
import spire.math.Rational

object GlobalImpl {
  implicit val format: Format[GlobalImpl] = AutoFormat[GlobalImpl]
}
case class GlobalImpl(crochets: Int = 32, dilations: Boolean = true) {
  def corpus: Vec[Cell] = if (dilations) Fitness.corpus else baseCells
  def duration = Rational(crochets, 4)
}
