package at.iem.point.eh.sketches

import scalax.chart.Charting._
import swing.{Swing, Frame}
import Swing._
import java.awt.GraphicsEnvironment

object FrameIntervalExample extends App {
  defer {
    val frames = List(11, 15, 19, 22).map(showFor)
    val b = GraphicsEnvironment.getLocalGraphicsEnvironment.getMaximumWindowBounds
    frames.zipWithIndex.foreach { case (f, i) =>
      f.location = (if (i % 2 == 0) b.x else b.x + b.width  - f.size.width) ->
                   (if (i / 2 == 0) b.y else b.y + b.height - f.size.height)
      f.open()
    }
  }

  def showFor(snippetIdx: Int): Frame = {
    val notes     = loadSnippet(snippetIdx).notes
    val chords    = ChordUtil.findChords(notes)
    val numChords = chords.size
    val fi        = chords.map(_.frameInterval)
    val fih       = fi.histogram
    val chordSize = {
      val set = chords.map(_.size).toSet
      if (set.size == 1) set.head else (set.min, set.max)
    }

    val fihData = fih.toXYSeriesCollection("Frame Interval Histogram")
    val chart   = XYBarChart(fihData)
//    chart.show()
    val f = chart.toFrame(title = s"Snippet #${snippetIdx} - No. of chords = ${numChords}, polyphony = ${chordSize}")
    f.pack()
    f
  }
}