package at.iem.point.sh.sketches
package gui

import java.awt._
import java.awt.geom.{Rectangle2D, Line2D}
import scala.swing.Swing
import Swing._

//trait ChromosomeView[C, G] {
//  def shape(chromosome: C): Shape
//  def geneView: GeneView[G]
//  def geneLocation(chromosome: C, gene: G): AffineTransform
//  def paint(chromosome: C, g: Graphics2D, width: Int, height: Int): Unit
//}

import Fitness._

object ChromosomeView {
  private val StabWidth           = 5.0
  private val PreferredScale      = 4 / (1.0/32)
  private val PreferredHeight     = 32
  private val PreferredNoteHeight = 16
  private val ColorStab           = Color.black
  private val ColorNote1          = Color.black
  private val ColorNote2          = Color.gray
  private val ColorRest1          = Color.lightGray
  private val ColorRest2          = new Color(224, 224, 224)
}
class ChromosomeView(c: Chromosome) {
  import ChromosomeView._

  private val cn        = c.map(_.normalized)
  private val totalDur  = cn.dur.toDouble

  private def hgrad(x1: Double, c1: Color, x2: Double, c2: Color): Paint =
    new LinearGradientPaint(x1.toFloat, 0f, x2.toFloat, 0f, Array(0f, 1f), Array(c1, c2))

  private def line(x1: Double, y1: Double, x2: Double, y2: Double): Shape = new Line2D     .Double(x1, y1, x2, y2)
  private def rect(x : Double, y : Double, w : Double, h : Double): Shape = new Rectangle2D.Double(x , y , w , h )

  def preferredSize: Dimension = {
    val w = (totalDur * PreferredScale + 0.5).toInt
    (w, PreferredHeight)
  }

  def paint(g: Graphics2D, width: Int, height: Int) {
    val sz    = cn.size
    if (sz == 0) return

    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING  , RenderingHints.VALUE_ANTIALIAS_ON)
    g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE )

    val stabs = (sz - 1) * StabWidth
    val netto = width - stabs
    val scale = netto / totalDur
    val nh    = math.min(PreferredNoteHeight, height * 0.5)
    val ny    = (height - nh) / 2

    cn.foldLeft(0.0) { (x, cell) =>
      val x1 = if (x > 0) {
        g.setPaint(ColorStab)
        val x0 = x + StabWidth/2 - 0.5
        g.draw(line(x0, 0, x0, height))
        x + StabWidth
      } else x

      val x3 = cell.elements.foldLeft(x1) { (x2, n) =>
        val nw = scale * n.dur.toDouble

        if (n.isRest) {
          g.setPaint(hgrad(x2, ColorRest1, x2 + nw, ColorRest2))
        } else {
          g.setPaint(hgrad(x2, ColorNote1, x2 + nw, ColorNote2))
        }
        g.fill(rect(x2 + 1, ny, nw - 2, nh))

        x2 + nw
      }

      x3
    }
  }
}