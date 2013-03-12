package at.iem.point.eh.sketches

import scalax.chart.Charting._
import swing.Swing
import Swing._
import org.jfree.chart.axis.{NumberAxis, NumberTickUnit}
import java.awt.Color

object HarmonicFields extends App {
  val weighted      = true
  val allIntervals  = true
  val writeOut      = true

  val f   = loadDefault(raw = false)
  val n   = f.notes
  val nf  = ChordUtil.findHarmonicFields(n)

  if (writeOut) {

  }

  val iv  = nf.flatMap { ch =>
    val res = if (allIntervals) ch.allIntervals else ch.layeredIntervals
    res.map(i => (i.semitones % 12, if (weighted) ch.avgDuration else 1.0))
  }

  var ivm = Map.empty[Int, Double] withDefaultValue(0.0)
  iv.foreach { case (i, dur) =>
    ivm += i -> (ivm(i) + dur)
  }

  val data  = ivm.map { case (i, dur) => (i, dur) } .toXYSeriesCollection("All intervals")
  val chart = XYBarChart(data, title =
    s"Histogram for${if (weighted) " duration-weighted" else ""} " +
    s"${if (allIntervals) "all" else "layered"} intervals"
  )

  defer {
    val plot  = chart.plot
    val rangeX  = plot.getDomainAxis.asInstanceOf[NumberAxis]
    plot.getRenderer.setSeriesPaint(0, Color.darkGray)
    rangeX.setTickUnit(new NumberTickUnit(1))
    if (!weighted) {
      val rangeY  = plot.getRangeAxis.asInstanceOf[NumberAxis]
      rangeY.setTickUnit(new NumberTickUnit(1))
    }
    val fr    = chart.toFrame()
    fr.size   = (700, 500)
    fr.centerOnScreen()
    fr.open()
  }
}