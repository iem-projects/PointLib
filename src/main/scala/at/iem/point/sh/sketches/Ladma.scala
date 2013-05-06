package at.iem.point.sh.sketches

// reference: http://vladimir_ladma.sweb.cz/english/music/articles/links/mrhythm.htm
object Ladma {
  def mobility(cell: Cell): Double = {
    val k     = cell.dur.denominator.toInt
    val num   = cell.dur.numerator.toLong // .toInt
    val prod  = cell.elements.map(_.dur * num).product  // a Long
    val avg   = math.pow(prod, 1.0/cell.elements.size)
    k.toDouble / avg
  }
}