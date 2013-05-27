package at.iem.point.sh.sketches

import collection.immutable.{IndexedSeq => IIdxSeq}
import at.iem.point.illism.rhythm.Ladma
import spire.syntax._

object FitnessApp extends App {
  import Fitness._

  implicit val r = rng(5L)

  val duration  = r"8"
  val pop       = 100
  val iter      = 20

  val win       = r"3/2"
  val step      = win/2

  def seqFit(seq: Sequence, w: Double): Double = {
    val e       = Ladma.entropy(seq.toCell)
    val target  = w.linlin(0, 1, 0.2, 2.0)
    // math.abs(e - target)
    math.abs(e - target) / target
  }

  def aggr(seq: IIdxSeq[Double]): Double = {
    val res = seq.sum / seq.size
    // println(s"aggr($seq) = $res")
    res
  }

  val fitnessSeq  = slidingFitnessByDuration(window = win, step = step)(fun = seqFit) _
  val fitness     = fitnessSeq.andThen(aggr _)

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
    val i     = r.nextInt(e1.size)  // splitting point
    val s1    = e1.take(i)          // left hand side
    val d1    = s1.dur
    val e2d   = e2.dur
    val fill  = duration - d1       // optimum duration of right-hand side
    val miss  = fill - e2d
    if (miss > 0) { // s1 (splitted e1) plus full e2 still too short, thus grow s1 and append e2
      val r1  = e1.drop(i).accumDur
      val t1  = r1.takeWhile { case (c, acc) => (acc - c.dur) < miss }
      val s1b = t1.optimumEnd(miss)(_._2) .drop_2
      s1b ++ e2

    } else if (fill == 0) { // s1 has perfect length
      s1

    } else {  // find s2, the optimium truncation of e2 at its beginning, and prepend s1

      val e2r   = e2.reverse
      val e2d   = e2r.accumDur
      val s2a   = e2d.takeWhile { case (n, acc) => (acc - n.dur) < fill }
      val s2    = s2a.optimumEnd(fill)(_._2) .drop_2.reverse  // optimumEnd on reversed seq is actually an optimum start
      s1 ++ s2
    }
  }

  val res     = produce(duration = duration, pop = pop, iter = iter)(fitness = fitness, selectAndBreed = selectAndBreed)
  val sorted  = res.distinct.sortBy(-_._2)

  sorted.take(5).foreach(println)
}