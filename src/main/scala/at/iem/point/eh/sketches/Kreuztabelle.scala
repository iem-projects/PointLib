package at.iem.point.eh.sketches

import swing.{Component, Swing}
import Swing._
import GUI._

object Kreuztabelle extends App {
  val DEBUG = false

  Swing.onEDT {
    run(chordSize = -1, intervalClasses = true)
  }

  def analyze(raw: Boolean = false, allIntervals: Boolean = false,
              intervalClasses: Boolean = false, chordSize: Int = -1): Component = {
    val f   = loadDefault(raw = raw)
    val n   = f.notes
    val nf0 = ChordUtil.findHarmonicFields(n)
    val nf  = if (chordSize < 0) nf0 else nf0.filter(_.size == chordSize)

//    var max = 0.0
    var mp  = Map.empty[Int, Map[Int, Int]] withDefaultValue(Map.empty withDefaultValue 0)

    nf.foreach { ch =>
      val iv = if (allIntervals) ch.allIntervals else ch.layeredIntervals
      for ((i,ii) <- iv.zipWithIndex; (j,jj) <- iv.zipWithIndex if ii < jj) {
        val x = if (intervalClasses) i.`class`.steps else i.semitones % 12
        val y = if (intervalClasses) j.`class`.steps else j.semitones % 12
        val xm = mp(x)
        val ym = mp(y)
        mp += x -> (xm + (y -> (xm(y) + 1)))
        mp += y -> (ym + (x -> (ym(x) + 1)))
      }
    }

//    println(mp)

    val title0 = s"File: ${if (raw) "raw" else "edited"} ${if (allIntervals) "all" else "layered"} interval ${if (intervalClasses) "classes " else ""}cross corr."
    val title  = if (chordSize < 0) title0 else s"$title0; sz=$chordSize"
    val panel  = ContinguencyChart(mp, if (intervalClasses) 7 else 12, title)
    panel
  }

  def run(chordSize: Int = -1, intervalClasses: Boolean = false) {
    val panes = for {
      raw          <- Seq(false, true)
      allIntervals <- Seq(true, false)
    } yield {
      analyze(raw = raw, allIntervals = allIntervals, chordSize = chordSize, intervalClasses = intervalClasses)
    }

    val panel = panes.asGrid(2, 2)
    frame("Kreuztabelle", panel, (1000, 1000))
  }
}