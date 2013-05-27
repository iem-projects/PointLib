package at.iem.point.sh.sketches

import collection.immutable.{IndexedSeq => IIdxSeq}
import at.iem.point.illism.rhythm.Ladma
import spire.syntax._

object FitnessApp extends App {
  import Fitness._

  implicit val r = rng(4L)

  val duration  = r"4"
  val pop       = 20
  val iter      = 1 // 20

  val win       = r"1"
  val step      = win/2

  def seqFit(seq: Sequence, w: Double): Double = {
    val e       = Ladma.entropy(seq.toCell)
    val target  = w.linlin(0, 1, 0.2, 2.0)
    math.abs(e - target)
  }

  def aggr(seq: IIdxSeq[Double]): Double = {
    val res = seq.sum / seq.size
    // println(s"aggr($seq) = $res")
    res
  }

  val fitness = slidingDuration(window = win, step = step)(fun = seqFit)(aggr = aggr) _

  def selectAndBreed(g: GenomeVal): Genome = {
    val sel   = truncationSelection(0.25)(g)
    Vector.fill(g.size) {
      val e1    = sel.choose()
      val e2    = sel.choose()
      val res   = cross(e1, e2)
      // println(s"cross($e1, $e2) = $res")
      res
    }
  }

  def cross(e1: Chromosome, e2: Chromosome): Chromosome = {
    val i     = r.nextInt(e1.size)
    val s1    = e1.take(i)
    val d1    = s1.dur
    val fill  = duration - d1
    val e2r   = e2.reverse
    val e2d   = e2r.accumDur
    val s2    = e2d.takeWhile(_._2 < fill).drop_2.reverse
    val d2    = s2.dur
    val sum   = d1 + d2
    if (sum >= duration) {
      if (s2.nonEmpty) {
        val r1 = sum / duration
        val r2 = duration / (sum - s2.head.dur)
        if (r1 < r2) {
          s1 ++ s2
        } else {
          s1 ++ s2.tail
        }
      } else {
        s1
      }
    } else {
      val rem   = e1.drop(i)
      val remd  = rem.accumDur // scanLeft(r"0")(_ + _.dur)
      val remf  = duration - sum
      val s3    = remd.takeWhile(_._2 < remf).drop_2
      val d3    = s3.dur
      val sum2  = sum + d3
      val s4    = if (s3.nonEmpty) {
        val r1    = sum2 / duration
        val sum2b = sum2 - s3.head.dur
        if (sum2b > 0 && r1 < duration / sum2b) {
          s1 ++ s3
        } else {
          s1 ++ s3.tail
        }
      } else {
        s1
      }
      s4 ++ s2
    }
  }

  val res = produce(duration = duration, pop = pop, iter = iter)(fitness = fitness, selectAndBreed = selectAndBreed)

  res.foreach(println)
}