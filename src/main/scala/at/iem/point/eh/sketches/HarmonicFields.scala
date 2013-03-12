package at.iem.point.eh.sketches

import scalax.chart.Charting._
import swing.{Component, GridPanel, Frame, Swing}
import Swing._
import org.jfree.chart.axis.{NumberAxis, NumberTickUnit}
import java.awt.{Font, Color}
import scalax.chart.XYChart
import org.jfree.chart.{ChartFactory, StandardChartTheme, ChartPanel}
import org.jfree.chart.title.TextTitle
import javax.swing.WindowConstants

object HarmonicFields extends App {
  Swing.onEDT(run())

  def analyse(raw: Boolean = false, weighted: Boolean = false, allIntervals: Boolean = false): XYChart = {
    val f   = loadDefault(raw = raw)
    val n   = f.notes
    val nf  = ChordUtil.findHarmonicFields(n)

    val iv  = nf.flatMap { ch =>
      val res = if (allIntervals) ch.allIntervals else ch.layeredIntervals
      res.map(i => (i.semitones % 12, if (weighted) ch.avgDuration else 1.0))
    }

    var ivm = Map.empty[Int, Double] withDefaultValue(0.0)
    iv.foreach { case (i, dur) =>
      ivm += i -> (ivm(i) + dur)
    }

    val data  = ivm.map { case (i, dur) => (i, dur) } .toXYSeriesCollection()
    val chart = XYBarChart(data, title =
      s"${if (raw) "Raw" else "Edited"} file: ${if (weighted) "weighted " else ""}" +
      s"${if (allIntervals) "all" else "layered"} intervals"
    )

    chart.peer.removeLegend()
    chart.peer.setTitle(new TextTitle(chart.title, new Font("SansSerif", java.awt.Font.BOLD, 12)))
    val plot  = chart.plot
    val rangeX  = plot.getDomainAxis.asInstanceOf[NumberAxis]
    plot.getRenderer.setSeriesPaint(0, Color.darkGray)
//    plot.getRenderer().setBarPainter(new StandardBarPainter())
    rangeX.setTickUnit(new NumberTickUnit(1))
    rangeX.setRange(-0.5, 11.5)
    if (!weighted) {
      val rangeY  = plot.getRangeAxis.asInstanceOf[NumberAxis]
      rangeY.setTickUnit(new NumberTickUnit(1))
    }

    chart
  }

  def run() {
    ChartFactory.setChartTheme(StandardChartTheme.createLegacyTheme())

    val charts = for {
      raw           <- Seq(false, true)
      allIntervals  <- Seq(false, true)
      weighted      <- Seq(false, true)
    }
    yield {
      analyse(raw = raw, weighted = weighted, allIntervals = allIntervals)
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
    PDFSupport.addMenu(fr.peer, panel.peer :: Nil, usePrefSize = false)

    fr.open()
  }
}