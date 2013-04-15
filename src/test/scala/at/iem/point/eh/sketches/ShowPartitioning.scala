package at.iem.point.eh.sketches

import at.iem.point.illism._
import at.iem.point.illism.gui.PianoRoll
import javax.swing.{KeyStroke, WindowConstants}
import scala.swing.{Action, BoxPanel, Component, Frame, BorderPanel, Orientation, Swing}
import Swing._
import de.sciss.audiowidgets.Axis
import scala.swing.event.ValueChanged
import de.sciss.desktop
import de.sciss.desktop.{KeyStrokes, FocusType}
import java.awt.event.KeyEvent

object ShowPartitioning extends App with Runnable {
  Swing.onEDT(run())

  def run() {
    val sn          = loadSnippet(improvSnippets(0))
    val dur         = sn.duration
    val (m, h)      = NoteUtil.splitMelodicHarmonic(sn.notes)
    val nm          = m.flatMap(_._2)
    val nh          = h.flatMap(_._2)
    val view        = PianoRoll.j()
    view.notes      = nm
    view.chords     = nh
    // val deco        = PianoRoll.NoteDecoration(Some(Color.red))
    // view.decoration = nh.map(n => n -> deco)(breakOut)
    val axis        = new Axis() {
      format        = Axis.Format.Time(hours = false, millis = true)
      minimum       = view.timeRange._1
      maximum       = view.timeRange._2
    }
    // val slider      = new JSlider((view.timeRange._1 * 100).toInt, (view.timeRange._2 * 100).toInt)
    lazy val scroll = new ScrollBarAlive {
      me =>

      orientation   = Orientation.Horizontal
      minimum       = 0
      maximum       = (dur * 1000).toInt

      listenTo(this)
      reactions += {
        case ValueChanged(_) =>
          val start = value * 0.001
          val stop  = start + visibleAmount * 0.001
          setSpan(start, stop)
      }
    }
    lazy val box = new BoxPanel(Orientation.Horizontal) {
      contents += HStrut(view.keyWidth)
      contents += axis
    }

    def setSpan(start: Double, stop: Double) {
      // println(s"setSpan($start, $stop)")
      view.timeRange        = (start, stop)
      axis.minimum          = start
      axis.maximum          = stop
      scroll.value          = (start * 1000).toInt
      scroll.visibleAmount  = math.max(1, ((stop - start) * 1000).toInt)
    }

    setSpan(0.0, math.min(dur, 20.0))

    import desktop.Implicits._
    import KeyStrokes._
    import KeyEvent._

    def action(stroke: KeyStroke)(body: => Unit) = new Action("Untitled") {
      accelerator = Some(stroke)
      def apply() { body }
    }

    scroll.addAction("zoom-out", action(menu2 + VK_LEFT) {
      val (start0, stop0) = view.timeRange
      val span0 = stop0 - start0
      val start = math.max(0.0, start0 - span0 * 0.5)
      val stop  = math.min(dur, start + span0 * 2)
      setSpan(start, stop)
    }, FocusType.Window)

    scroll.addAction("zoom-in", action(menu2 + VK_RIGHT) {
      val (start0, stop0) = view.timeRange
      val span0 = stop0 - start0
      if (span0 >= 1.0) {
        val start = start0 + span0 * 0.25
        val stop  = start  + span0 * 0.5
        setSpan(start, stop)
      }
    }, FocusType.Window)

    scroll.addAction("zoom-all-out", action(menu1 + VK_LEFT) {
      setSpan(0.0, dur)
    }, FocusType.Window)

    new Frame {
      contents = new BorderPanel {
        add(Component.wrap(view), BorderPanel.Position.Center)
        add(box,    BorderPanel.Position.North)
        add(scroll, BorderPanel.Position.South)
      }
      pack()
      centerOnScreen()
      peer.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
      open()
    }
  }
}