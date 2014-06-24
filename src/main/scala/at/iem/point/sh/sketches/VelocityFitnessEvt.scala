package at.iem.point.sh.sketches

import Fitness._
import spire.syntax.literals._
import de.sciss.numbers.Implicits._

object VelocityFitnessEvt extends FitnessLike {
  lazy val veloStart  = r"1/4" // r"1/32" // r"1/64"
  lazy val veloStop   = r"1/4" // r"1/2"  // r"1/1"

  override lazy val title     = "Increasing GeomMean"
  override lazy val subTitle  = s"$veloStart to $veloStop using sliding window of $evt events across duration of $duration"
  override lazy val pop       = 10000 // 200
  override lazy val iter      = 1
  override lazy val duration  = r"16"

  override lazy val seed      = 7L
  override lazy val evt       = 4

  def seqFit(seq: Sequence, w: Double): Double = {
    val seq1    = seq.bindTrailingRests

    // the geometric average is n-th root of the product of the durations
    val prod    = seq1.product
    val e       = math.pow(prod.toDouble, 1.0/seq1.size)
    val v1      = veloStart.toDouble
    val v2      = veloStop .toDouble
    val target  = if (v1 == v2) {
      v1
    } else {
      w.linexp(0, 1, v1, v2).linexp(v1, v2, v1, v2) // double exponential because of decreasing note density
    }
    math.abs(e - target) / target
  }

  run()
}