package at.iem.point.sh.sketches
package genetic

import collection.immutable.{IndexedSeq => Vec}
import Fitness._
import scala.annotation.tailrec
import scala.util.Random

object Selection {
  // cf. http://en.wikipedia.org/wiki/Fitness_proportionate_selection
  case class Roulette(size: SelectionSize = SelectionSize.Percentage()) extends Selection {
    override def apply(pop: GenomeVal, rnd: util.Random): Genome = {
      val n = size(pop.size)

      @tailrec def loop(rem: Int, in: GenomeVal, out: Genome): Genome = if (rem == 0) out else {
        val sum     = in.view.map(_._2).sum
        val rem1    = rem - 1
        if (sum == 0) {
          val ((head, _) +: tail) = in
          loop(rem1, tail, out :+ head)
        } else {
          val norm        = in.map { case (c, f) => (c, f / sum) }
          val sorted      = norm.sortBy(_._2)
          val roul        = rnd.nextDouble()
          val accum       = sorted.scanLeft(0.0) { case (a, (_, f)) => a + f }
          assert(accum.last == 1.0) // XXX TODO: this will probably fail due to floating point noise
          val idx         = accum.indexWhere(_ > roul)
          val (chosen, _) = in(idx)
          val in1         = in.patch(idx, Vec.empty, 1)
          loop(rem1, in1, out :+ chosen)
        }
      }

      loop(n, pop, Vec.empty)
    }
  }

  // cf. http://en.wikipedia.org/wiki/Truncation_selection
  case class Truncation(size: SelectionSize = SelectionSize.Percentage()) extends Selection {
    override def apply(pop: GenomeVal, rnd: Random): Genome = {
      val n       = size(pop.size)
      val sorted  = pop.sortBy(_._2)
      sorted.takeRight(n).map(_._1)
    }
  }
}
sealed trait Selection extends ((GenomeVal, util.Random) => Genome)

object SelectionSize {

  /** Selects an absolute number of individuals
    *
    * @param value  the number of individuals to select
    */
  case class Number(value: Int = 10) extends SelectionSize {
    require(value > 0)
    override def apply(pop: Int): Int = math.min(pop, value)
  }
  /** Selects the number of individuals corresponding to
    * a given percentage of the total population.
    *
    * @param value  the percentage value ranging from 0 to 100
    */
  case class Percentage(value: Int = 20) extends SelectionSize {
    require(value > 0 && value <= 100)
    override def apply(pop: Int): Int = pop * value / 100
  }
}
sealed trait SelectionSize extends (Int => Int)