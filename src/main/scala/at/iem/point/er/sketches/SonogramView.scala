package at.iem.point.er.sketches

import de.sciss.sonogram.{SonogramOverview, SimpleSonogramView}
import java.awt.{Point, Graphics2D, Color, Graphics}
import java.awt.event.MouseEvent
import javax.swing.event.MouseInputAdapter
import de.sciss.synth

final class SonogramView extends SimpleSonogramView {
  private val colrTransp = new Color(0xFF, 0xFF, 0xFF, 0x40)

  private var mousePt = Option.empty[Point]

  private def screenToTime(screen: Float, ovr: SonogramOverview): Float = {
    import synth._
    val spec  = ovr.fileSpec
    val w     = getWidth
    screen.linlin(0, w, 0, spec.numFrames/spec.sampleRate).toFloat
  }

  private def screenToFreq(screen: Float, ovr: SonogramOverview): Float = {
    import synth._
    val spec  = ovr.fileSpec.sono
    val h     = getHeight
    screen.linexp(h - 1, 0, spec.minFreq, spec.maxFreq).toFloat
  }

  private object mouse extends MouseInputAdapter {
    private def process(e: MouseEvent) {
      mousePt = Some(e.getPoint)
      sono.foreach { ovr =>
        val time  = screenToTime(e.getX, ovr)
        val freq  = screenToFreq(e.getY, ovr)
        setToolTipText(f"time = $time%1.3f s, freq = $freq%1.1f Hz")
      }
      repaint()
    }

    override def mouseEntered(e: MouseEvent) {
      process(e)
    }

    override def mouseExited(e: MouseEvent) {
      mousePt = None
      repaint()
    }

    override def mouseDragged(e: MouseEvent) {
      process(e)
    }

    override def mouseMoved(e: MouseEvent) {
      process(e)
    }
  }

  addMouseListener(mouse)
  addMouseMotionListener(mouse)

  override def paintComponent(g: Graphics) {
    super.paintComponent(g)
    mousePt.foreach { pt =>
      val g2 = g.asInstanceOf[Graphics2D]
      g2.setColor(colrTransp)
      val w   = getWidth
      val wm  = w - 1
      val h   = getHeight
      val hm  = h - 1
      g2.drawLine(0, pt.y, wm, pt.y)
      g2.drawLine(pt.x, 0, pt.x, hm)
    }
  }
}