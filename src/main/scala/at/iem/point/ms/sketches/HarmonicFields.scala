package at.iem.point.ms.sketches

import scalax.chart.Charting._
import swing.{Component, GridPanel, Frame, Swing}
import Swing._
import org.jfree.chart.axis.{NumberAxis, NumberTickUnit}
import java.awt.{Font, Color}
import scalax.chart.XYChart
import org.jfree.chart.{ChartFactory, StandardChartTheme, ChartPanel}
import org.jfree.chart.title.TextTitle
import javax.swing.WindowConstants
import at.iem.point.illism._
import de.sciss.pdflitz
import de.sciss.midi

object HarmonicFields extends App {
  Swing.onEDT {
    run(chordSize = -1, intervalClasses = true)
  }

  def analyse(raw: Boolean = false, weighted: Boolean = false, allIntervals: Boolean = false,
              intervalClasses: Boolean = false, chordSize: Int = -1): XYChart = {
    val st  = if (raw) Study.Raw(0) else Study.Edited(0)
    val f   = load(st)
    val title = s"${if (raw) "Raw" else "Edited"} file: ${if (weighted) "weighted " else ""}" +
                s"${if (allIntervals) "all" else "layered"} ${if (intervalClasses) "interval classes" else "intervals"}"
    apply(f, weighted = weighted, allIntervals = allIntervals, intervalClasses = intervalClasses,
      chordSize = chordSize, title = title)
  }

  def apply(seq: midi.Sequence, weighted: Boolean = false, allIntervals: Boolean = false,
            intervalClasses: Boolean = false, chordSize: Int = -1, title: String = "Title"): XYChart = {
    val n   = seq.notes
    val nf0 = ChordUtil.findHarmonicFields(n)
    val nf  = if (chordSize < 0) nf0 else nf0.filter(_.size == chordSize)

    val iv  = nf.flatMap { ch =>
      val res = if (allIntervals) ch.allIntervals else ch.layeredIntervals
      res.map { i =>
        val steps   = if (intervalClasses) i.`class`.steps else i.semitones % 12
        val weight  = if (weighted) ch.avgDuration else 1.0
        (steps, weight)
      }
    }

    var ivm = Map.empty[Int, Double] withDefaultValue 0.0
    iv.foreach { case (i, dur) =>
      ivm += i -> (ivm(i) + dur)
    }

    val data  = ivm.map { case (i, dur) => (i, dur) } .toXYSeriesCollection()
    val chart = XYBarChart(data, title = title)

    chart.peer.removeLegend()
    chart.peer.setTitle(new TextTitle(chart.title, new Font("SansSerif", java.awt.Font.BOLD, 12)))
    val plot  = chart.plot
    val rangeX  = plot.getDomainAxis.asInstanceOf[NumberAxis]
    plot.getRenderer.setSeriesPaint(0, Color.darkGray)
    //    plot.getRenderer().setBarPainter(new StandardBarPainter())
    rangeX.setTickUnit(new NumberTickUnit(1))
    rangeX.setRange(-0.5, (if (intervalClasses) 7 else 12) - 0.5)
    if (!weighted) {
      val rangeY  = plot.getRangeAxis.asInstanceOf[NumberAxis]
      rangeY.setTickUnit(new NumberTickUnit(1))
    }

    chart
  }

  def run(chordSize: Int = -1, intervalClasses: Boolean = false) {
    ChartFactory.setChartTheme(StandardChartTheme.createLegacyTheme())

    val charts = for {
      raw           <- Seq(false, true)
      allIntervals  <- Seq(false, true)
      weighted      <- Seq(false, true)
    }
    yield {
      analyse(raw = raw, weighted = weighted, allIntervals = allIntervals, intervalClasses = intervalClasses,
        chordSize = chordSize)
    }

    val panel = new GridPanel(2, 4) {
      contents ++= charts.map(c => Component.wrap(new ChartPanel(c.peer, false))) // no offscreen buffering
    }

    val fr = new Frame {
      title     = "Harmonic Fields"
      contents  = panel
      size      = (1200, 900)
      centerOnScreen()
      peer.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    }
    new pdflitz.SaveAction(panel :: Nil).setupMenu(fr)
    fr.open()
  }
}