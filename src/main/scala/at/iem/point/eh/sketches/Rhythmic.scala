package at.iem.point.eh.sketches

object Rhythmic extends App {
  for (raw <- Seq(true, false)) {
    println(s"---- In ${if (raw) "raw" else "edited"} file: ----")
    val f     = loadDefault(raw = raw)
    for (ch <- -1 until 4 ) {
      val n1    = f.notes(channel = ch)
      val tFlt  = NoteUtil.clean(n1)
      val stabs = NoteUtil.stabbings(tFlt)
      val durs  = stabs.pairDiff
      val (mean, v) = durs.meanVariance
      println( f"For ${if (ch < 0) "all voices" else "voice " + (ch+1)}, mean rhythmic cell duration is $mean%1.2f sec, std. dev is ${math.sqrt(v)}%1.2f sec.")
    }
  }
}