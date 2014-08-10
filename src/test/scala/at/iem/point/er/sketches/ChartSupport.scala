package at.iem.point.er.sketches

import org.jfree.chart.ChartPanel
import org.jfree.chart.plot.{CategoryPlot, XYPlot}
import scalax.chart.Chart
import java.awt.{Rectangle, Font, Color}
import org.jfree.chart.renderer.xy.{StandardXYBarPainter, XYBarRenderer}
import scala.swing.{Component, Frame, Swing}
import scala.swing.event.WindowClosing
import de.sciss.pdflitz.Generate.QuickDraw
import de.sciss.pdflitz
import Swing._

object ChartSupport {
  implicit class RichChart(chart: Chart) {
    /** Adjust the chart with a black-on-white color scheme and
      * fonts that come out properly in PDF export.
      */
    def printableLook(): Unit = {
      val plot = chart.plot

      val (xAxis, yAxis) = plot match {  // shitty Plot / Renderer interfaces do not have common super types
        case p: XYPlot       =>
          p.setBackgroundPaint           (Color.white    )
          p.setDomainGridlinePaint       (Color.lightGray)
          p.setRangeGridlinePaint        (Color.lightGray)
          p.getRenderer.setSeriesPaint(0, Color.black /* darkGray */)
          // undo the crappy "3D" look
          p.getRenderer match {
            case r: XYBarRenderer => r.setBarPainter(new StandardXYBarPainter())
            case _ =>
          }
          (p.getDomainAxis, p.getRangeAxis)
        case p: CategoryPlot =>
          p.setBackgroundPaint           (Color.white    )
          p.setDomainGridlinePaint       (Color.lightGray)
          p.setRangeGridlinePaint        (Color.lightGray)
          p.getRenderer.setSeriesPaint(0, Color.darkGray )
          // undo the crappy "3D" look
          p.getRenderer match {
            case r: XYBarRenderer => r.setBarPainter(new StandardXYBarPainter())
            case _ =>
          }
          (p.getDomainAxis, p.getRangeAxis)
      }

      //      val xAxis         = plot.getDomainAxis
      //      val yAxis         = plot.getRangeAxis
      val fnt1          = new Font("Helvetica", Font.BOLD , 14)
      val fnt2          = new Font("Helvetica", Font.PLAIN, 12)
      xAxis.setLabelFont(fnt1)
      xAxis.setTickLabelFont(fnt2)
      yAxis.setLabelFont(fnt1)
      yAxis.setTickLabelFont(fnt2)
    }
  }

  def drawAction(chart: Chart, w: Int, h: Int) = QuickDraw(w -> h) { g =>
    // g.setColor(Color.red)
    // g.fillRect(0, 0, w, h)

    chart.peer.draw(g, new Rectangle(0, 0, w, h))
  }

  def showChart(chart: Chart, w: Int, h: Int, frameTitle: String = ""): Unit = {
    // val p = chart.toPanel
    val pp = new ChartPanel(chart.peer)
    val p  = Component.wrap(pp)
    pp.setMouseWheelEnabled(true) // SO #19281374
    val f = new Frame {
      contents = p
        if (frameTitle.nonEmpty) title = frameTitle

      listenTo(this)
      reactions += {
        case WindowClosing(_) => sys.exit()
      }
    }

    val draw = drawAction(chart, w, h)
    new pdflitz.SaveAction(draw :: Nil).setupMenu(f)

    f.pack()
    f.centerOnScreen()
    f.open()
  }
}