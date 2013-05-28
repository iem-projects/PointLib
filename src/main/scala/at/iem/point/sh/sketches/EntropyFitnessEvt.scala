package at.iem.point.sh.sketches

import Fitness._
import at.iem.point.illism.rhythm.Ladma

object EntropyFitnessEvt extends FitnessLike {
  lazy val evt      = 5
  lazy val evtStep  = evt/2

  override lazy val title     = "Increasing Entropy"
  override lazy val subTitle  = s"0.2 to 2.0 using sliding window of $evt events"

  def seqFit(seq: Sequence, w: Double): Double = {
    val e       = Ladma.entropy(seq.toCell)
    val target  = w.linexp(0, 1, 0.2, 2.0)
    // math.abs(e - target)
    math.abs(e - target) / target
  }

  override lazy val fitnessSeq  = slidingFitnessByEvents(window = evt, step = evtStep)(fun = seqFit) _

  run()
}