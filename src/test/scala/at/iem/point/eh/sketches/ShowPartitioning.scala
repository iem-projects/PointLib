package at.iem.point.eh.sketches

import at.iem.point.illism._
import at.iem.point.illism.gui.impl.PianoRollImpl
import javax.swing.{KeyStroke, WindowConstants}
import scala.swing.{Action, BoxPanel, Component, Frame, BorderPanel, Orientation, Swing}
import Swing._
import de.sciss.{audiowidgets, desktop, midi}
import audiowidgets.{Transport, Axis}
import scala.swing.event.{MouseDragged, MousePressed, ValueChanged}
import desktop.{KeyStrokes, FocusType}
import java.awt.event.KeyEvent
import java.awt.{Graphics, Point, Color, RenderingHints, Graphics2D}
import java.awt.geom.GeneralPath

object ShowPartitioning extends App with Runnable {
  Swing.onEDT(run())

  def run() {
    val sn          = loadSnippet(improvSnippets(1))
    val notes       = sn.notes
    val dur         = sn.duration
    val (m, h)      = NoteUtil.splitMelodicHarmonic(notes)
    val nm          = m.flatMap(_._2)
    val nh          = h.flatMap(_._2)

    var position    = 0.0
    val colr        = new Color(0x00, 0x00, 0xFF, 0x80)

    val view        = new PianoRollImpl.JComponent {
      override def paintComponent(g: Graphics) {
        super.paintComponent(g)
        val (start, stop) = timeRange
        val x = keyWidth + ((position - start) / (stop - start) * (getWidth - keyWidth) + 0.5).toInt
        g.setColor(colr)
        g.drawLine(x, 0, x, getHeight - 1)
      }
    }
    view.notes      = nm
    view.chords     = nh
    // val deco        = PianoRoll.NoteDecoration(Some(Color.red))
    // view.decoration = nh.map(n => n -> deco)(breakOut)
    lazy val axis   = new Axis() {
      format        = Axis.Format.Time(hours = false, millis = true)
      minimum       = view.timeRange._1
      maximum       = view.timeRange._2

      val tri = new GeneralPath()
      tri.moveTo(-6, 0)
      tri.lineTo(7, 0)
      tri.lineTo(0, 13)
      tri.closePath()

      listenTo(mouse.clicks)
      listenTo(mouse.moves)

      def move(pt: Point, mod: Int) {
        val p = math.max(0.0, math.min(dur, (pt.x.toDouble / size.width) * (maximum - minimum) + minimum))
        setPos(p)
      }

      reactions += {
        case MousePressed(_, pt, mod, _, _) => move(pt, mod)
        case MouseDragged(_, pt, mod)       => move(pt, mod)
      }

      override def paintComponent(g: Graphics2D) {
        super.paintComponent(g)
        val x = (position - minimum) / (maximum - minimum) * size.width
        g.setColor(colr)
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
        val atOrig = g.getTransform
        g.translate(x, 0)
        g.fill(tri)
        g.setTransform(atOrig)
      }
    }

    def setPos(p: Double) {
      position = p
      axis.repaint()
      view.repaint()
    }

    // val slider      = new JSlider((view.timeRange._1 * 100).toInt, (view.timeRange._2 * 100).toInt)
    lazy val scroll = new ScrollBarAlive {
      me =>

      orientation   = Orientation.Horizontal
      minimum       = 0
      maximum       = (dur * 1000).toInt
      unitIncrement = 100

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
      scroll.blockIncrement = scroll.visibleAmount * 3/4
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

    val box2 = new BoxPanel(Orientation.Horizontal) {
      contents += scroll
      contents += HStrut(16)
    }

    lazy val sequencer = midi.Sequencer.open()

    var playStartPos  = 0.0
    var playStartTime = 0L
    val timer = new javax.swing.Timer(50, ActionListener { _ =>
      val p = playStartPos + (System.currentTimeMillis() - playStartTime) * 0.001
      setPos(p)
    })

    def play() {
      stop()
      val sq  = if (position == 0) sn else {
        val n0  = notes.dropWhile(_.offset < position)
        implicit val rate = sn.rate
        val n1  = n0.map(n => n.copy(offset = n.offset - position))
        val t   = midi.Track(n1.flatMap(_.toMIDI))
        midi.Sequence(Vector(t))
      }
      sequencer.play(sq)
      playStartPos  = position
      playStartTime = System.currentTimeMillis()
      timer.restart()
    }

    def stop() {
      timer.stop()
      if (sequencer.isPlaying) sequencer.stop()
    }

    def rtz() {
      setPos(0.0)
    }

    import Transport._
    val transp = Transport.makeButtonStrip(GoToBegin(rtz()) :: Play(play()) :: Stop(stop()) :: Nil)

    transp.addAction("play-stop", action(plain + VK_SPACE) {
      if (sequencer.isPlaying) stop() else play()
    }, FocusType.Window)

    val box3 = new BoxPanel(Orientation.Vertical) {
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += HGlue
        contents += transp
      }
      contents += box
    }

    new Frame {
      contents = new BorderPanel {
        add(Component.wrap(view), BorderPanel.Position.Center)
        add(box3, BorderPanel.Position.North)
        add(box2, BorderPanel.Position.South)
      }
      pack()
      centerOnScreen()
      peer.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
      open()
    }
  }
}