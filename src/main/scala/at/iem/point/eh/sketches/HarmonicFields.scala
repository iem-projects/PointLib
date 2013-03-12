package at.iem.point.eh.sketches

object HarmonicFields extends App {
  val f   = loadDefault()
  val n   = f.notes
  val nf  = ChordUtil.findHarmonicFields(n)
  nf.foreach(println)
}