package at.iem.point.sh.sketches

import Fitness._
import at.iem.point.illism.rhythm.Ladma
import spire.syntax.literals._
import de.sciss.numbers.Implicits._

object TensionFitnessEvt extends FitnessLike {
  override lazy val title     = "Decreasing Tension"
  override lazy val subTitle  = s"1.5 to 0.1 (exp) using sliding window of $evt events across duration of $duration"
  override lazy val pop       = 200
  override lazy val duration  = r"16"

  override lazy val seed      = 6L

  def seqFit(seq: Sequence, w: Double): Double = {
    val e       = Ladma.tension(seq.toCell)
    val target  = w.linexp(1, 0, 0.1, 1.5)
    // math.abs(e - target)
    math.abs(e - target) / target
  }

  run()
}