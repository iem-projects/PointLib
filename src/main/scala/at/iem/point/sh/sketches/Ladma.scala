package at.iem.point.sh.sketches

import scala.collection.immutable.{IndexedSeq => IIdxSeq}
import spire.math._

// reference: http://vladimir_ladma.sweb.cz/english/music/articles/links/mrhythm.htm
object Ladma {
  private def prepare(cell: Cell): (IIdxSeq[Rational], BigInt) = {
    // we first incorporate the total duration into the cell's elements
    // (e.g [1, 1, 1], 1/4 becomes [1/12, 1/12, 1/12], 1
    val dursN   = cell.normalized.elements.map(_.dur)
    // we find the least common multiple of the elements' denominators.
    // this is the order k of rhythmical system
    val k       = dursN.map(_.denominator).reduce(lcm(_, _))
    // the measure is based on the relative durations (e.g. multiplied by the order)
    val dursM   = dursN.map(_ * k)
    (dursM, k)
  }

  def mobility(cell: Cell): Double = {
    val (dursM, k) = prepare(cell)
    // the geometric average is n-th root of the product of the relative durations
    val prod    = dursM.product
    val avg     = math.pow(prod.toDouble, 1.0/cell.elements.size)
    // mobility is order divided by average
    k.toDouble / avg
  }

  def tension(cell: Cell): Double = {
    val (dursM, k) = prepare(cell)
    // Ladma uses the arithmetic mean instead of the geometric mean here.
    val mean    = dursM.sum / dursM.size
    val sqrdif  = dursM.map { d => val dif = d - mean; dif * dif }
    val msqrdif = sqrdif.sum / dursM.size
    val stddev  = math.sqrt(msqrdif.toDouble)
    // tension is two times standard deviation divided by order
    2 * stddev / k.toDouble
  }

  def entropy(cell: Cell): Double = {
    val (dursM, k) = prepare(cell)
    // Note: Ladma uses symbol `k` twice. The sum obviously goes over the number of elements
    // and not the order of the structure.
    val ps = dursM.map { d => val quot = (d/k).toDouble; quot * math.log(quot) }
    val h  = -ps.sum
    h
  }
}