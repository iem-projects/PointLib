package at.iem.point.er.sketches

import de.sciss.file._
import de.sciss.synth.io.AudioFile
import scalax.chart.{ChartFactories, Charting}
import scala.swing.Swing
import ChartSupport._
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import java.awt.BasicStroke

object CrossSimilarityPlot extends App {
  lazy val simFile    = baseDir / "struga_out" / "test2.aif"

  lazy val smoothing  = 48
  lazy val fftSize    = 1024
  lazy val fftOverlap = 2
  lazy val sr         = 44100.0 / (fftSize/fftOverlap)
  lazy val crossLen   = 9.0 // seconds
  lazy val tMin       = 20.0 // 0.0 // time axis minimum in seconds
  lazy val tMax       = 30.0 // 10.0 // 430.0 // .13991 // time axis minimum in seconds
  lazy val decimation = 4

  run()

  def run(): Unit = {
    val af        = AudioFile.openRead(simFile)

    try {
      val len = af.numFrames.toInt
      // println(len)
      val b   = af.buffer(len)
      af.read(b)
      // b(0).view.filter(_ > 0)
      val b0      = b(0)
      val medianH = smoothing // 24
      val flt     = b0.sliding(medianH * 2 + 1).map { win => /* win.max */ win.sorted.apply(medianH) }
      val smp     = flt /* b(0) */ /* .iterator */ .zipWithIndex.collect { case (f, i) if f > 0 => (f, i) } .toIndexedSeq

      // println(s"Len: ${b(0).length}; ${smp.size}")
      val (minY, _) = smp.minBy(_._1)
      val (maxY, _) = smp.maxBy(_._1)
      // println(s"Min: ${smp.minBy(_._1)}; Max: ${smp.maxBy(_._1)}")

      val timeOff   = crossLen/2

      import Charting._
      val norm = smp.map { case (f, i) =>
        // import de.sciss.numbers.Implicits._
        val t = i / sr + timeOff
        t -> f // f.linlin(minY, maxY, 0, 1).max(0)
      }

      // val (minX, _) = norm.minBy(_._1)
      // val (maxX, _) = norm.maxBy(_._2)

      val norm1 = norm.zipWithIndex.collect { case (e, i) if i % decimation == 0 => e }
      val coll  = norm1.toXYSeriesCollection()

      Swing.onEDT {
        val chart = ChartFactories.XYLineChart(coll, legend = false)
        val plot  = chart.plot
        val yAxis = plot.getRangeAxis
        val xAxis = plot.getDomainAxis
        val r = plot.getRenderer.asInstanceOf[XYLineAndShapeRenderer]
        // val r = new XYSplineRenderer
        // plot.setRenderer(r)
        r.setDrawSeriesLineAsPath(true)   // !
        r.setBaseShapesFilled(false)
        r.setSeriesShapesFilled(0, false)
        r.setBaseShapesVisible(false)
        // r.setBaseStroke(new BasicStroke(2f))
        // r.setBaseOutlineStroke(new BasicStroke(2f))
        r.setSeriesStroke(0, new BasicStroke(2f)) // , BasicStroke.CAP_BUTT, BasicStroke.JOIN_ROUND, 4f))
        yAxis.setRange(minY, maxY)
        yAxis.setVisible(false)
        xAxis.setRange(tMin, tMax)
        xAxis.setVisible(false)
        chart.printableLook()
        showChart(chart, 600, 400)
      }

    } finally {
      af.close()
    }
  }
}
