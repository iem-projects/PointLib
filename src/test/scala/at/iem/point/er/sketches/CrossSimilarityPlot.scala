package at.iem.point.er.sketches

import de.sciss.file._
import de.sciss.synth.io.AudioFile
import scalax.chart.{ChartFactories, Charting}
import scala.swing.Swing
import ChartSupport._
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import java.awt.{Color, BasicStroke}
import org.jfree.data.xy.XYSeriesCollection
import de.sciss.pdflitz

object CrossSimilarityPlot extends App {
  def test = false

  lazy val simFile    = baseDir / "struga_out" / "test2.aif"

  lazy val smoothing  = 48
  lazy val fftSize    = 1024
  lazy val fftOverlap = 2
  lazy val sr         = 44100.0 / (fftSize/fftOverlap)
  lazy val crossLen   = 9.0 // seconds
  lazy val useOffset  = false
  // lazy val tMin       = 20.0 // 0.0 // time axis minimum in seconds
  // lazy val tMax       = 30.0 // 10.0 // 430.0 // .13991 // time axis minimum in seconds
  lazy val decimation = 4
  lazy val width      = 860
  lazy val height     = 368

  lazy val outFiles   = baseDir / "sim_out" / "heidelberger1a_sim"

  run()

  case class Range(min: Double, max: Double)

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

      val timeOff   = if (useOffset) crossLen/2 else 0.0

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
        implicit val range = Range(minY, maxY)
        plotAll(coll)
      }
    } finally {
      af.close()
    }
  }

  private def plotAll(coll: XYSeriesCollection)(implicit range: Range): Unit = {
    for (page <- 0 until (if (test) 1 else 8)) {
      for (system <- 0 until (if (test) 1 else 3)) {
        plotSystem(coll, page, system)
      }
    }
    println("Done.")
  }

  private def plotSystem(coll: XYSeriesCollection, page: Int, system: Int)(implicit range: Range): Unit = {
    // val tMin  = (page * 3 + system) * 10.0
    // val tMax  = tMin + 10.0
    val timeIdx = page * 3 + system
    val tMin    = scoreTimes(timeIdx)
    val tMax    = scoreTimes(timeIdx + 1)

    val chart = ChartFactories.XYLineChart(coll, legend = false)
    val plot  = chart.plot
    val yAxis = plot.getRangeAxis
    val xAxis = plot.getDomainAxis
    val r = plot.getRenderer.asInstanceOf[XYLineAndShapeRenderer]
    // plot.setOutlineVisible(false)
    plot.setOutlinePaint(new Color(0x00, 0x00, 0x00, 0x3F)) // semi transparent
    // val r = new XYSplineRenderer
    // plot.setRenderer(r)
    r.setDrawSeriesLineAsPath(true)   // !
    r.setBaseShapesFilled(false)
    r.setSeriesShapesFilled(0, false)
    r.setBaseShapesVisible(false)
    // r.setBaseStroke(new BasicStroke(2f))
    // r.setBaseOutlineStroke(new BasicStroke(2f))
    r.setSeriesStroke(0, new BasicStroke(2f)) // , BasicStroke.CAP_BUTT, BasicStroke.JOIN_ROUND, 4f))
    yAxis.setRange(range.min, range.max)
    yAxis.setVisible(false)
    xAxis.setRange(tMin, tMax)
    xAxis.setVisible(false)
    chart.printableLook()
    chart.peer.setBackgroundPaint(new Color(0xFF, 0xFF, 0xFF, 0))
    plot.setBackgroundPaint(new Color(0xFF, 0xFF, 0xFF, 0x00))
    //plot.setBackgroundAlpha(0f)
    // showChart(chart, 600, 400)
    val draw = drawAction(chart, width, height)
    val outFile = outFiles.parent / s"${outFiles.name}_p${page + 1}s${system + 1}.pdf"
    println(s"Saving '${outFile.name}'...")
    pdflitz.Generate(outFile, draw, overwrite = test)
  }
}
