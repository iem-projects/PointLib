package at.iem.point.sh.sketches

import Fitness._
import at.iem.point.illism.rhythm.Ladma

object EntropyFitness extends FitnessLike {
  override lazy val title     = "Increasing Entropy"
  override lazy val subTitle  = s"0.2 to 2.0 using sliding window of duration $win"

  def seqFit(seq: Sequence, w: Double): Double = {
    val e       = Ladma.entropy(seq.toCell)
    val target  = w.linlin(0, 1, 0.2, 2.0)
    // math.abs(e - target)
    math.abs(e - target) / target
  }

  override lazy val fitnessSeq  = slidingFitnessByDuration(window = win, step = step)(fun = seqFit) _

  run()
}