package at.iem.point.ms.sketches

import swing.{Component, Swing}
import Swing._
import GUI._
import at.iem.point.illism._

object Kreuztabelle extends App {
  def DEBUG = false

  Swing.onEDT {
    run(chordSize = -1, intervalClasses = true, idx = 11)
  }

  def analyze(study: Study, allIntervals: Boolean = false,
              intervalClasses: Boolean = false, chordSize: Int = -1): Component = {
    val f   = load(study)
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

    val title0 = s"${study.file.name}: ${if (allIntervals) "all" else "layered"} interval ${if (intervalClasses) "classes " else ""}cross corr."
    val title  = if (chordSize < 0) title0 else s"$title0; sz=$chordSize"
    val panel  = ContinguencyChart(mp, if (intervalClasses) 7 else 12, title)
    panel
  }

  /** `idx` is zero for the file in `MIDI`, and >1 for those in `MIDI3` */
  def run(chordSize: Int = -1, intervalClasses: Boolean = false, idx: Int = 0) {
    val rawSeq = if (idx == 0) Seq(false, true) else Seq(true)
    val panes = for {
      raw          <- rawSeq
      allIntervals <- Seq(true, false)
    } yield {
      val study = if (raw) Study.Raw(idx) else Study.Edited(idx)
      analyze(study, allIntervals = allIntervals, chordSize = chordSize, intervalClasses = intervalClasses)
    }

    val panel = panes.asGrid(2, 2)
    frame("Kreuztabelle", panel, (1000, 1000))
  }
}