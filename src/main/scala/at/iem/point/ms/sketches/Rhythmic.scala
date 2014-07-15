/*
 *  Rhythmic.scala
 *  (PointLib - ms)
 *
 *  Copyright (c) 2013-2014 IEM Graz / Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package at.iem.point.ms.sketches

import at.iem.point.illism._

object Rhythmic extends App {
  for (raw <- Seq(true, false)) {
    println(s"---- In ${if (raw) "raw" else "edited"} file: ----")
    val study = if (raw) Study.Raw(0) else Study.Edited(0)
    val f     = load(study)
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