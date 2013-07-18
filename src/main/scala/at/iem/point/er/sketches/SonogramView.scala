package at.iem.point.er.sketches

import de.sciss.sonogram
import java.awt.event.MouseEvent
import javax.swing.event.MouseInputAdapter
import de.sciss.synth
import java.awt.geom.{GeneralPath, Rectangle2D}
import java.awt.{RenderingHints, Graphics, Graphics2D, Point, BasicStroke, Color}
import de.sciss.audiowidgets.{TimelineModel, TimelineCanvas}
import scala.swing.Swing
import Swing._
import de.sciss.audiowidgets.impl.TimelineCanvasImpl
import de.sciss.model.Change

final class SonogramView(doc: Document, canvas: TimelineCanvas) extends sonogram.SonogramComponent {
  private val colrCrosshair = new Color(0xFF, 0xFF, 0xFF, 0x40)
  //  private val colrPitch     = new Color(0x40, 0x40, 0xFF, 0x80)
  private val colrPitch     = new Color(0xFF, 0xFF, 0x00, 0xA0)
  private val colrPitchOut  = new Color(0x00, 0x00, 0xFF, 0xA0)
  private val strkPitchOut  = new BasicStroke(2f)
  private val strkOnsets    = new BasicStroke(1f)
  private val strkCrosshair = new BasicStroke(1f)

  private var mousePt = Option.empty[Point]

  private var _pitch  = Vec.empty[PitchAnalysis.Sample]
  private var _onsets = MultiResOnsets.empty

  //  def pitchOverlay = _pitch
  //  def pitchOverlay_=(value: PitchAnalysis.Product) {
  //    _pitch = value
  //    repaint()
  //  }
  //
  //  def onsetsOverlay = _onsets
  //  def onsetsOverlay_=(value: MultiResOnsets) {
  //    _onsets = value
  //    repaint()
  //  }

  //  private def screenToTime(screen: Float, ovr: sonogram.Overview): Float = {
  //    import synth._
  //    val spec  = ovr.inputSpec
  //    val w     = getWidth
  //    screen.linlin(0, w, 0, spec.numFrames/spec.sampleRate).toFloat
  //  }

  //  private def secsToScreen(secs: Float, ovr: SonogramOverview): Float = {
  //    import synth._
  //    val spec  = ovr.fileSpec
  //    val w     = getWidth
  //    secs.linlin(0, spec.numFrames/spec.sampleRate, 0, w).toFloat
  //  }

  //  private def frameToScreen(frame: Long, ovr: sonogram.Overview): Float = {
  //    import synth._
  //    val spec  = ovr.inputSpec
  //    val w     = getWidth
  //    frame.toDouble.linlin(0, spec.numFrames, 0, w).toFloat
  //  }

  private def screenToFreq(screen: Float, ovr: sonogram.Overview): Float = {
    import synth._
    val spec  = ovr.config.sonogram
    val h     = getHeight
    screen.linexp(h - 1, 0, spec.minFreq, spec.maxFreq).toFloat
  }

  private def freqToScreen(freq: Float, ovr: sonogram.Overview): Float = {
    val spec  = ovr.config.sonogram
    val h     = getHeight
    freq.toDouble.explin(spec.minFreq, spec.maxFreq, h - 1, 0).toFloat
  }

  private object mouse extends MouseInputAdapter {
    private def process(e: MouseEvent) {
      mousePt = Some(e.getPoint)
      sono.foreach { ovr =>
        val sr    = canvas.timelineModel.sampleRate
        val time  = canvas.screenToFrame(e.getX) / sr
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
  canvas.timelineModel.addListener {
    case TimelineModel.Visible(_, _) => repaint()
  }
  doc.addListener {
    case Document.OnsetsChanged(_, Change(_, now)) =>
      _onsets = now
      repaint()

    case Document.PitchesChanged(_, Change(_, now)) =>
      _pitch = now
      repaint()
  }

  private val shpRect = new Rectangle2D.Float()
  private val shpPath = new GeneralPath()

  override def paintComponent(g: Graphics) {
    // super.paintComponent(g)
    val g2 = g.asInstanceOf[Graphics2D]
    g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION,  RenderingHints.VALUE_INTERPOLATION_BILINEAR)

    val w    = getWidth
    val h    = getHeight

    sono.foreach { ovr =>
      g2.setPaint(if (ovr.isCompleted) Color.black else TimelineCanvasImpl.pntChecker)
      g2.fillRect(0, 0, w, h)

      val sp = canvas.timelineModel.visible
      ovr.paint(sp.start, sp.stop, g2, 0, 0, w, h, this)

      if (_pitch.nonEmpty) {
        //      g.setColor(colrPitch)
        //      g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
        //      g2.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_NORMALIZE)
        g2.setStroke(strkPitchOut)
        _pitch.foreach { smp =>
          val x1  = canvas.frameToScreen(smp.start)
          val x2  = canvas.frameToScreen(smp.stop )
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

            case CurveFitting.QuadraticFit(_, _, _, _) => ???

          }
          g2.setColor(colrPitchOut)
          g2.draw(shp)
          g2.setColor(colrPitch)
          g2.fill(shp)
        }
      }

      if (_onsets.nonEmpty) {
        g2.setStroke(strkOnsets)
        val lvl = _onsets.levels
        _onsets.onsets.foreach { entry =>
          val frame = entry.pos
          val x  = canvas.frameToScreen(frame).toInt
          val h1 = getHeight - 1
          val y1 = entry.from * h1 / lvl
          val y2 = entry.to   * h1 / lvl
          g2.setColor(colrPitchOut)
          g2.fillRect(x-1, y1, 3, y2 - y1)
          g2.setColor(colrPitch)
          g2.drawLine(x, y1, x, y2)
          //          g2.setColor(colrPitchOut)
          //          g2.drawLine(x+1, y1, x+1, y2)
        }
      }
    }

    // canvas.paint....

    mousePt.foreach { pt =>
      g2.setColor(colrCrosshair)
      g2.setStroke(strkCrosshair)
      val wm  = w - 1
      val hm  = h - 1
      g2.drawLine(0, pt.y, wm, pt.y)
      g2.drawLine(pt.x, 0, pt.x, hm)
    }
  }

  setPreferredSize((600, 400))
}