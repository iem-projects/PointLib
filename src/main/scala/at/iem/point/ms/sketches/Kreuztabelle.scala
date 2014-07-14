/*
 *  Kreuztabelle.scala
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

import swing.{Component, Swing}
import Swing._
import GUI._
import at.iem.point.illism._
import de.sciss.midi
import de.sciss.file._
import de.sciss.kollflitz.Ops._

object Kreuztabelle extends App {
  def DEBUG = false

  type M = Map[Int, Map[Int, Int]]

  Swing.onEDT {
    run(chordSize = -1, intervalClasses = true, idx = 11)
  }

  def analyzeAndPlot(study: Study, allIntervals: Boolean = false,
                     intervalClasses: Boolean = false, chordSize: Int = -1, horiz: Boolean = false): Component = {
    val map     = analyze(study, allIntervals = allIntervals, intervalClasses = intervalClasses, chordSize = chordSize,
                                 horiz = horiz)
    val title0  = s"${study.file.name}: ${if (allIntervals) "all" else "layered"} interval ${if (intervalClasses) "classes " else ""}cross corr."
    val title   = if (chordSize < 0) title0 else s"$title0; sz=$chordSize"
    plotData(map, intervalClasses = intervalClasses, title = title)
  }

  def analyze(study: StudyLike, allIntervals: Boolean = false,
              intervalClasses: Boolean = false, chordSize: Int = -1, horiz: Boolean = false): M = {
    val f = load(study)
    apply(f, allIntervals = allIntervals, intervalClasses = intervalClasses, chordSize = chordSize, horiz = horiz)
  }

  def apply(seq: midi.Sequence, allIntervals: Boolean = false,
            intervalClasses: Boolean = false, chordSize: Int = -1, horiz: Boolean = false): M = {
    val n   = seq.notes
    val nf0 = ChordUtil.findHarmonicFields(n)
    val nf  = if (chordSize < 0) nf0 else nf0.filter(_.size == chordSize)

    // var max = 0.0
    var mp  = Map.empty[Int, Map[Int, Int]] withDefaultValue(Map.empty withDefaultValue 0)

    def addToMap(iv: Vec[UndirectedInterval]): Unit =
      for ((i,ii) <- iv.zipWithIndex; (j,jj) <- iv.zipWithIndex if ii < jj) {
        val x = if (intervalClasses) i.`class`.steps else i.semitones % 12
        val y = if (intervalClasses) j.`class`.steps else j.semitones % 12
        val xm = mp(x)
        val ym = mp(y)
        mp += x -> (xm + (y -> (xm(y) + 1)))
        mp += y -> (ym + (x -> (ym(x) + 1)))
      }

    if (horiz) {
      nf.mapPairs { (ch1, ch2) =>
        val p1s = ch1.pitches
        val p2s = ch2.pitches
        assert (p1s.size == p2s.size, s"Pred has ${p1s.size} voices, succ has ${p2s.size} voices")
        val iv = (p1s zip p2s).map { case (p1, p2) =>
          (p1 to p2).undirected
        }
        addToMap(iv)
      }

    } else {
      nf.foreach { ch =>
        val iv = if (allIntervals) ch.allIntervals else ch.layeredIntervals
        addToMap(iv)
      }
    }

    mp
  }

  def plot(seq: midi.Sequence, allIntervals: Boolean = false,
              intervalClasses: Boolean = false, chordSize: Int = -1, title: String = "Title"): Component = {
    val mp  = apply(seq, allIntervals = allIntervals, intervalClasses = intervalClasses, chordSize = chordSize)
    plotData(mp, intervalClasses = intervalClasses, title = title )
  }

  private def plotData(mp: M, intervalClasses: Boolean, title: String = "Title"): Component = {
    val panel = ContinguencyChart(mp, if (intervalClasses) 7 else 12, title)
    panel
  }

  /** `idx` is zero for the file in `MIDI`, and >1 for those in `MIDI3` */
  def run(chordSize: Int = -1, intervalClasses: Boolean = false, idx: Int = 0): Unit = {
    val rawSeq = if (idx == 0) Seq(false, true) else Seq(true)
    val panes = for {
      raw          <- rawSeq
      allIntervals <- Seq(true, false)
    } yield {
      val study = if (raw) Study.Raw(idx) else Study.Edited(idx)
      analyzeAndPlot(study, allIntervals = allIntervals, chordSize = chordSize, intervalClasses = intervalClasses)
    }

    val panel = panes.asGrid(2, 2)
    frame("Kreuztabelle", panel, (1000, 1000))
  }
}