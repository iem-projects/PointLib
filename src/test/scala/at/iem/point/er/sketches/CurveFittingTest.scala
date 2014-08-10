package at.iem.point.er.sketches

import swing.Swing
import scalax.chart._
import api._
import java.awt.Color
import org.jfree.chart.renderer.xy.StandardXYItemRenderer

object CurveFittingTest extends App {
  val n     = 20
  val ys    = List.fill(n)(math.random * 100 - 50).tails.map(_.sum).toList
  val pt    = ys.zipWithIndex.map(_.swap)
  val ptc   = pt.map { case (x, y) => CurveFitting.Point(x,y) }
  val qfit  = CurveFitting.solveQuadratic(ptc)
  val qpt   = Vector.tabulate(pt.size * 10) { i => val x = i * 0.1; x -> qfit(x) }
  val lfit  = CurveFitting.solveLinear(ptc)
  val lpt   = Vector.tabulate(pt.size * 10) { i => val x = i * 0.1; x -> lfit(x) }
  val pfit  = CurveFitting.solvePoint(ptc)
  val ppt   = Vector.tabulate(pt.size * 10) { i => val x = i * 0.1; x -> pfit(x) }

  println(qfit)
  println(lfit)
  println(pfit)

  Swing.onEDT {
    val data  = pt.toXYSeriesCollection("measured")
    val dq    = qpt.toXYSeriesCollection("quadratic fit")
    val dl    = lpt.toXYSeriesCollection("linear fit")
    val dp    = ppt.toXYSeriesCollection("point fit")
    val chart = XYLineChart(null)
    val plot  = chart.plot
    plot.setDataset(0, data)
    plot.setDataset(1, dq)
    plot.setDataset(2, dl)
    plot.setDataset(3, dp)
//    plot.getRenderer.setSeriesPaint(0, Color.blue)
//    plot.getRenderer.setSeriesPaint(1, Color.red)
    val r0 = new StandardXYItemRenderer()
    r0.setSeriesPaint(0, Color.black)
    plot.setRenderer(0, r0)
    val r1 = new StandardXYItemRenderer()
    r1.setSeriesPaint(0, Color.red)
    plot.setRenderer(1, r1)
    val r2 = new StandardXYItemRenderer()
    r2.setSeriesPaint(0, new Color(0x00, 0xC0, 0x00))
    plot.setRenderer(2, r2)
    val r3 = new StandardXYItemRenderer()
    r3.setSeriesPaint(0, new Color(0x20, 0x40, 0xFF))
    plot.setRenderer(3, r3)

    val f = chart.toFrame("Curve Fitting")
    f.pack()
    f.centerOnScreen()
    f.open()
  }
}