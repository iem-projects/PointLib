package at.iem.point.eh.sketches

import org.jfree.data.xy.{MatrixSeriesCollection, MatrixSeries}
import org.jfree.chart.{ChartPanel, JFreeChart}
import org.jfree.chart.plot.XYPlot
import org.jfree.chart.renderer.GrayPaintScale
import org.jfree.chart.renderer.xy.XYBlockRenderer
import java.awt.{Font, Graphics2D, Color}
import org.jfree.chart.axis.NumberAxis
import scalax.chart.XYChart
import swing.{GridPanel, Component, Frame, Swing}
import Swing._
import javax.swing.WindowConstants

object Kreuztabelle extends App {
  val DEBUG = false

  Swing.onEDT(run())

  def analyze(raw: Boolean = false, allIntervals: Boolean = false): (XYChart, Map[Int, Map[Int, Int]], MatrixSeries) = {
    val f   = loadDefault(raw = raw)
    val n   = f.notes
    val nf  = ChordUtil.findHarmonicFields(n)

    val m   = new MatrixSeries("Interval Correlations", 12, 12)
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

    // normalize
    for(i <- 0 until 12) {
      val maxi = (0 until 12).map(jj => mp(i)(jj)).max
      for(j <- 0 until 12) {
        val maxj = (0 until 12).map(ii => mp(ii)(j)).max
        val max = math.max(maxi, maxj)
        val n0 = if (max > 0) 1.0 - (mp(i)(j).toDouble / max) else 1.0
        val n = n0 // math.pow(n0, 2)
        m.update(i, j, n)
      }
    }

    val coll      = new MatrixSeriesCollection(m)
    val renderer  = new XYBlockRenderer()
    val scale     = new GrayPaintScale(0.0, 1.0)
    renderer.setPaintScale(scale)
    val xAxis     = new NumberAxis("Semitones")
    xAxis.setStandardTickUnits(NumberAxis.createIntegerTickUnits())
    xAxis.setLowerMargin(0.0)
    xAxis.setUpperMargin(0.0)
    val yAxis     = new NumberAxis("Semitones")
    yAxis.setStandardTickUnits(NumberAxis.createIntegerTickUnits())
    yAxis.setLowerMargin(0.0)
    yAxis.setUpperMargin(0.0)
    val plot      = new XYPlot(coll, xAxis, yAxis, renderer)
    plot.setBackgroundPaint(Color.lightGray)
    plot.setDomainGridlinesVisible(false)
    plot.setRangeGridlinePaint(Color.white)
    val title = s"File: ${if (raw) "raw" else "edited"} ${if (allIntervals) "all" else "layered"} interval cross corr."
    val chart     = new JFreeChart(title, plot)
    chart.removeLegend()
    chart.setBackgroundPaint(Color.white)
    val wrap = new XYChart {
      override val peer = chart
    }
    (wrap, mp, m)
  }

  def run() {
    val panes = for {
      raw          <- Seq(false, true)
      allIntervals <- Seq(true, false)
    } yield {
      val (chart, mp, m) = analyze(raw = raw, allIntervals = allIntervals)
      val fnt = new Font("SansSerif", Font.BOLD, 18)
      val panel = new Component {
        override lazy val peer = new ChartPanel(chart.peer, false) with SuperMixin
        override protected def paintComponent(g: Graphics2D) {
          super.paintComponent(g)
          // g.drawString("Schoko", 10, 30)
          val xoff    = 49
          val right   = 7
          val yoff    = 28
          val bottom  = 39
          val w       = peer.getWidth - (xoff + right)
          val h       = peer.getHeight - (yoff + bottom)

          if (DEBUG) {
            g.setColor(Color.red)
            g.drawLine(xoff, yoff, xoff + 100, yoff)
            g.drawLine(xoff, yoff, xoff, yoff + 100)
            g.drawLine(xoff + w - 1, yoff + h - 1, xoff + w - 100, yoff + h - 1)
            g.drawLine(xoff + w - 1, yoff + h - 1, xoff + w - 1, yoff + h - 100)
          }

          g.setFont(fnt)
          val fm = g.getFontMetrics
          for (i <- 0 until 12) {
            for (j <- 0 until 12) {
              g.setColor(if (m.get(i, j) > 0.5) Color.black else Color.white)
              val cnt = mp(i)(j)
              if (cnt > 0) {
                val str = cnt.toString
                val x   = (i + 0.5) / 12 * w - (fm.stringWidth(str) * 0.5) + xoff
                val y   = (1.0 - ((j + 0.5) / 12)) * h + 6 /* + (fm.getAscent * 0.5) */ + yoff
                g.drawString(str, x.toFloat, y.toFloat)
              }
            }
          }
        }
      }
      panel
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