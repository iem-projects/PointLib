package at.iem.point.sh.sketches

import spire.math._

// reference: http://vladimir_ladma.sweb.cz/english/music/articles/links/mrhythm.htm
object Ladma {
  def mobility(cell: Cell): Double = {
    // we first incorporate the total duration into the cell's elements
    // (e.g [1, 1, 1], 1/4 becomes [1/12, 1/12, 1/12], 1
    val dursN   = cell.normalized.elements.map(_.dur)
    // we find the least common multiple of the elements' denominators.
    // this is the order k of rhythmical system
    val k       = dursN.map(_.denominator).reduce(lcm(_, _))
    // the mobility is based on the relative durations (e.g. multiplied by the order)
    val dursM   = dursN.map(_ * k)
    // the geometric average is n-th root of the product of the relative durations
    val prod    = dursM.product
    val avg     = math.pow(prod.toDouble, 1.0/cell.elements.size)
    // mobility is order divided by average
    k.toDouble / avg
  }
}