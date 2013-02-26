package at.iem.point.er.sketches

import swing.Swing
import scalax.chart._
import Charting._
import java.awt.Color
import org.jfree.chart.renderer.xy.StandardXYItemRenderer

object CurveFittingTest extends App {
  val n     = 20
  val ys    = Vector.fill(n)(math.random * 100)
  val pt    = ys.zipWithIndex.map(_.swap)
  val fit   = CurveFitting.solveQuadratic(pt.map { case (x, y) => CurveFitting.Point(x,y) })
  val fitp  = Vector.tabulate(pt.size * 10) { i => val x = i * 0.1; x -> fit(x) }

  println(fit)

  Swing.onEDT {
    val data  = pt.toXYSeriesCollection("measured")
    val dfit  = fitp.toXYSeriesCollection("fitted")
    val chart = XYLineChart(null)
    val plot  = chart.plot
    plot.setDataset(0, data)
    plot.setDataset(1, dfit)
//    plot.getRenderer.setSeriesPaint(0, Color.blue)
//    plot.getRenderer.setSeriesPaint(1, Color.red)
    val r1 = new StandardXYItemRenderer()
    r1.setSeriesPaint(0, Color.blue)
    plot.setRenderer(0, r1)
    val r2 = new StandardXYItemRenderer()
    r2.setSeriesPaint(0, Color.red)
    plot.setRenderer(1, r2)

    val f = chart.toFrame("Curve Fitting")
    f.pack()
    f.centerOnScreen()
    f.open()
  }
}