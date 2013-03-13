package at.iem.point.eh.sketches

object Rhythmic extends App {
  val f     = loadDefault()
  for (ch <- 0 until 4 ) {
    val n1    = f.notes(channel = ch)
    val tFlt  = NoteUtil.clean(n1)
    val stabs = NoteUtil.stabbings(tFlt)
//    val durs  = stabs.pairDiff
    val (mean, v) = stabs.meanVariance
    println( f"For voice ${ch+1}%d, mean rhythmic cell duration is ${mean}%1.2f sec, std. dev is ${math.sqrt(v)}%1.2f sec.")
  }
}