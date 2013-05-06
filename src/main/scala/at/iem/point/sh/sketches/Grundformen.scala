package at.iem.point.sh.sketches

import java.awt.Color
import scalax.chart.XYChart
import scalax.chart.Charting._
import org.jfree.chart.ChartPanel
import scala.swing.{Component, Frame, SimpleSwingApplication}
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import scala.swing.event.WindowClosing
import org.jfree.chart.labels.StandardXYItemLabelGenerator

object Grundformen extends SimpleSwingApplication {
  // either of "m", "t", "e", "t/m"
  val xaxis = "e"
  val yaxis = "t/m"

  lazy val top = {
    import Cell._
    import Ladma._

    val m = cell.map(mobility)
    val t = cell.map(tension)
    val e = cell.map(entropy)

    // val data  = m zip t
    // val datas = data.sortBy { case (a, b) => a * b }

    def parse(s: String) = s match {
      case "m"   => (m, "Mobility", Some(-0.1))
      case "t"   => (t, "Tension", None)
      case "e"   => (e, "Entropy", None)
      case "t/m" => ((t zip m).map { case (t0, m0) => math.exp(t0/m0) }, "exp Tension/Mobility", Some(0.9))
    }

    val (xdata, xlabel, _)   = parse(xaxis)
    val (ydata, ylabel, ylo) = parse(yaxis)

    val data  = xdata zip ydata

    val chart = mkChart(data, ylo, title = "Grundformen", datalabel = "Cell", xlabel = xlabel, ylabel = ylabel)
    mkFrame(chart)
  }

  def mkFrame(chart: XYChart): Frame = {
    val panelj  = new ChartPanel(chart.peer, false)
    val panel   = Component.wrap(panelj)
    new Frame {
      contents = panel
      PDFSupport.addMenu(peer, panel.peer :: Nil, usePrefSize = false)
      pack().centerOnScreen()
      listenTo(this)
      reactions += {
        case WindowClosing(_) => quit()
      }
      open()
    }
  }

  def mkChart(data: Iterable[(Double, Double)], ylo: Option[Double],
              datalabel: String = "Data", xlabel: String = "X", ylabel: String = "y",
              title: String = "Title"): XYChart = {
    val fihData = data.toXYSeriesCollection(datalabel)
    val chart   = XYLineChart(fihData, title = title)
    val plot    = chart.plot

    // val rangeX  = plot.getDomainAxis.asInstanceOf[NumberAxis]
    // rangeX.setTickUnit(new NumberTickUnit(1))
    val rangeY  = plot.getRangeAxis // .asInstanceOf[NumberAxis]
    ylo.foreach(rangeY.setLowerBound(_))
    // rangeY.setTickUnit(new NumberTickUnit(1))

    // doesn't do what we wish:
    // plot.getDomainAxis.setLowerMargin(0.05)
    plot.getDomainAxis.setLabel(xlabel)
    plot.getRangeAxis .setLabel(ylabel)

    plot.setBackgroundPaint(Color.white)
    val renderer = new XYLineAndShapeRenderer()
    renderer.setSeriesLinesVisible(0, false)
    renderer.setSeriesShapesVisible(0, true)
    renderer.setSeriesPaint(0, Color.darkGray)

    renderer.setBaseItemLabelsVisible(true)
    renderer.setBaseItemLabelGenerator(new StandardXYItemLabelGenerator() {
      override def generateLabel(dataset: XYDataset, series: Int, item: Int) = (item + 1).toString
    })

    plot.setRenderer(renderer)

    chart
  }
}