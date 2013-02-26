package at.iem.point.er.sketches

import de.sciss.sonogram.{SonogramOverview, SimpleSonogramView}
import java.awt.event.MouseEvent
import javax.swing.event.MouseInputAdapter
import de.sciss.synth
import collection.immutable.{IndexedSeq => IIdxSeq}
import java.awt.geom.{GeneralPath, Rectangle2D}
import java.awt.{Graphics, Graphics2D, Point, BasicStroke, Color}

final class SonogramView extends SimpleSonogramView {
  private val colrCrosshair = new Color(0xFF, 0xFF, 0xFF, 0x40)
//  private val colrPitch     = new Color(0x40, 0x40, 0xFF, 0x80)
  private val colrPitch     = new Color(0xFF, 0xFF, 0x00, 0xA0)
  private val colrPitchOut  = new Color(0x00, 0x00, 0xFF, 0xA0)
  private val strkPitchOut  = new BasicStroke(2f)

  private var mousePt = Option.empty[Point]

  private var _pitch = IIdxSeq.empty[PitchAnalysis.Sample]

  def pitchOverlay = _pitch
  def pitchOverlay_=(value: PitchAnalysis.PayLoad) {
    _pitch = value
    repaint()
  }

  private def screenToTime(screen: Float, ovr: SonogramOverview): Float = {
    import synth._
    val spec  = ovr.fileSpec
    val w     = getWidth
    screen.linlin(0, w, 0, spec.numFrames/spec.sampleRate).toFloat
  }

//  private def secsToScreen(secs: Float, ovr: SonogramOverview): Float = {
//    import synth._
//    val spec  = ovr.fileSpec
//    val w     = getWidth
//    secs.linlin(0, spec.numFrames/spec.sampleRate, 0, w).toFloat
//  }

  private def frameToScreen(frame: Long, ovr: SonogramOverview): Float = {
    import synth._
    val spec  = ovr.fileSpec
    val w     = getWidth
    frame.toDouble.linlin(0, spec.numFrames, 0, w).toFloat
  }

  private def screenToFreq(screen: Float, ovr: SonogramOverview): Float = {
    import synth._
    val spec  = ovr.fileSpec.sono
    val h     = getHeight
    screen.linexp(h - 1, 0, spec.minFreq, spec.maxFreq).toFloat
  }

  private def freqToScreen(freq: Float, ovr: SonogramOverview): Float = {
    val spec  = ovr.fileSpec.sono
    val h     = getHeight
    freq.toDouble.explin(spec.minFreq, spec.maxFreq, h - 1, 0).toFloat
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

  private val shpRect = new Rectangle2D.Float()
  private val shpPath = new GeneralPath()

  override def paintComponent(g: Graphics) {
    super.paintComponent(g)
    val g2 = g.asInstanceOf[Graphics2D]

    if (_pitch.nonEmpty) {
//      g.setColor(colrPitch)
//      g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
//      g2.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_NORMALIZE)
      g2.setStroke(strkPitchOut)
      sono.foreach { ovr =>
        _pitch.foreach { smp =>
          val x1  = frameToScreen(smp.start, ovr)
          val x2  = frameToScreen(smp.stop,  ovr)
          val shp = smp.freq match {
            case CurveFitting.PointFit(freq)  =>
              val y   = freqToScreen(freq.toFloat, ovr)
              shpRect.setRect(x1 - 1, y - 3, x2 - x1 + 2, 6)
              shpRect

            case lf @ CurveFitting.LinearFit(_, startFreq) =>
              shpPath.reset()
              val y1  = freqToScreen(startFreq.toFloat, ovr)
              val y2  = freqToScreen(lf(smp.stop-smp.start-1).toFloat, ovr)
              shpPath.reset()
              shpPath.moveTo(x1 - 1, y1 - 3)
              shpPath.lineTo(x2 + 1, y2 - 3)
              shpPath.lineTo(x2 + 1, y2 + 3)
              shpPath.lineTo(x1 - 1, y1 + 3)
              shpPath.closePath()
              shpPath

//            case CurveFitting.QuadraticFit(_, _, _, _) =>

          }
          g2.setColor(colrPitchOut)
          g2.draw(shp)
          g2.setColor(colrPitch)
          g2.fill(shp)
        }
      }
    }

    mousePt.foreach { pt =>
      g.setColor(colrCrosshair)
      val w   = getWidth
      val wm  = w - 1
      val h   = getHeight
      val hm  = h - 1
      g.drawLine(0, pt.y, wm, pt.y)
      g.drawLine(pt.x, 0, pt.x, hm)
    }
  }
}