package at.iem.point.eh.sketches

import swing.{GridPanel, Component, Frame, Swing}
import Swing._
import javax.swing.WindowConstants

object Kreuztabelle extends App {
  val DEBUG = false

  Swing.onEDT(run(-1))

  def analyze(raw: Boolean = false, allIntervals: Boolean = false,
              chordSize: Int = -1): Component = {
    val f   = loadDefault(raw = raw)
    val n   = f.notes
    val nf0 = ChordUtil.findHarmonicFields(n)
    val nf  = if (chordSize < 0) nf0 else nf0.filter(_.size == chordSize)

//    var max = 0.0
    var mp  = Map.empty[Int, Map[Int, Int]] withDefaultValue(Map.empty withDefaultValue 0)

    nf.foreach { ch =>
      val iv = if (allIntervals) ch.allIntervals else ch.layeredIntervals
      for ((i,ii) <- iv.zipWithIndex; (j,jj) <- iv.zipWithIndex if ii < jj) {
        val x = i.semitones % 12
        val y = j.semitones % 12
//        val c = m.get(y, x) + 1
//          if (c > max) max = c
//        m.update(y, x, c)
        val xm = mp(x)
        val ym = mp(y)
        mp += x -> (xm + (y -> (xm(y) + 1)))
        mp += y -> (ym + (x -> (ym(x) + 1)))
      }
    }

//    println(mp)

    val title0 = s"File: ${if (raw) "raw" else "edited"} ${if (allIntervals) "all" else "layered"} interval cross corr."
    val title  = if (chordSize < 0) title0 else s"$title0; sz=$chordSize"
    val panel  = ContinguencyChart(mp, 12, title)
    panel
  }

  def run(chordSize: Int = -1) {
    val panes = for {
      raw          <- Seq(false, true)
      allIntervals <- Seq(true, false)
    } yield {
      analyze(raw = raw, allIntervals = allIntervals, chordSize = chordSize)
    }

    val panel = new GridPanel(2, 2) {
      contents ++= panes
    }

    val fr = new Frame {
      title = "Kreuztabelle"
      contents = panel
      size = (1000, 1000)
      peer.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
      centerOnScreen()
    }
    PDFSupport.addMenu(fr.peer, panel.peer :: Nil, usePrefSize = false)
    fr.open()
  }
}