package at.iem.point.sh.sketches

import Fitness._
import at.iem.point.illism.rhythm.Ladma
import spire.syntax._

object MobilityFitnessEvt extends FitnessLike {
  override lazy val title     = "Increasing Mobility"
  override lazy val subTitle  = s"10 to 40 (exp) using sliding window of $evt events across duration of $duration"
  override lazy val pop       = 200
  override lazy val duration  = r"16"

  override lazy val seed      = 7L
  override lazy val evt       = 7

  def seqFit(seq: Sequence, w: Double): Double = {
    val e       = Ladma.mobility(seq.toCell)
    val target  = w.linexp(0, 1, 10, 40)
    // math.abs(e - target)
    math.abs(e - target) / target
  }

  run()
}