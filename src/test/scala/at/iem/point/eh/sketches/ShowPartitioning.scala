package at.iem.point.eh.sketches

import at.iem.point.illism._
import at.iem.point.illism.gui.impl.PianoRollImpl
import javax.swing.{KeyStroke, WindowConstants}
import scala.swing.{Label, Action, BoxPanel, Component, Frame, BorderPanel, Orientation, Swing}
import Swing._
import de.sciss.{desktop, midi}
import de.sciss.audiowidgets.{AxisFormat, LCDColors, LCDPanel, Transport, Axis}
import scala.swing.event.{MouseDragged, MousePressed, ValueChanged}
import desktop.{KeyStrokes, FocusType}
import java.awt.event.KeyEvent
import java.awt.{Graphics, Point, Color, RenderingHints, Graphics2D}
import java.awt.geom.GeneralPath
import at.iem.point.illism.gui.PianoRoll.NoteDecoration
import collection.breakOut
import de.sciss.pdflitz
import de.sciss.swingplus.ScrollBar

trait ShowPartitioning {
  def show(notes: Vec[Vec[OffsetNote]], chords: Vec[Vec[Chord]], numGroups: Int = -1)
          (implicit rate: midi.TickRate): Unit = {
    val notesF      = notes.flatten
    val chordsF     = chords.flatten
    val allNotes    = (notesF ++ chordsF.flatMap(_.notes)).sortBy(_.offset)
    val dur         = if (allNotes.isEmpty) 1.0 else allNotes.maxBy(_.stop).stop

    println(s"Notes ${notesF.size} chords ${chordsF.size}")

    var position    = 0.0
    val colr        = new Color(0x00, 0x00, 0xFF, 0x80)

    val lcd         = new Label {
      foreground = LCDColors.defaultFg
    }

    val lcdP = new LCDPanel {
      contents += lcd
      background  = LCDColors.defaultBg
    }

    val view        = new PianoRollImpl.JComponent {
      override def paintComponent(g: Graphics): Unit = {
        super.paintComponent(g)
        val (start, stop) = timeRange
        val x = keyWidth + ((position - start) / (stop - start) * (getWidth - keyWidth) + 0.5).toInt
        g.setColor(colr)
        g.drawLine(x, 0, x, getHeight - 1)
      }
    }
    view.notes      = notesF
    view.chords     = chordsF
    val pch         = allNotes.map(_.pitch.midi)
    val (minPch, maxPch) = (pch.min, pch.max)
    view.pitchRange = (minPch - (minPch % 12)) -> ((maxPch + 11) - ((maxPch + 11) % 12))

    val noteGroupSz   = if (numGroups < 0) notes.size else numGroups
    val chordGroupSz  = chords.size
    if (noteGroupSz > 1 || chordGroupSz > 1) {  // use decorators
      val ngs1    = noteGroupSz - 1
      val hueMin  = if (numGroups < 0) 0.40f else 0.2f
      val hueMax  = if (numGroups < 0) 0.70f else 0.8f
      val m: Map[OffsetNote, NoteDecoration] = notes.zipWithIndex.flatMap({ case (ns, i) =>
        val in    = (i % noteGroupSz).toFloat // / ngs1
        val colrN = Color.getHSBColor(in.linlin(0, ngs1, hueMin, hueMax), 0.75f, in.linlin(0, ngs1, 0.75f, 1.0f))
        val decN  = NoteDecoration(Some(colrN))
        ns.map(n => n -> decN)(breakOut) // : Map[OffsetNote, NoteDecoration]
      })(breakOut)
      view.decoration = m
    }

    val timeFormat  = AxisFormat.Time(hours = false, millis = true)

    lazy val axis   = new Axis() {
      format        = timeFormat // AxisFormat.Time(hours = false, millis = true)
      minimum       = view.timeRange._1
      maximum       = view.timeRange._2

      val tri = new GeneralPath()
      tri.moveTo(-6,  0)
      tri.lineTo( 7,  0)
      tri.lineTo( 0, 13)
      tri.closePath()

      listenTo(mouse.clicks)
      listenTo(mouse.moves)

      def move(pt: Point, mod: Int): Unit = {
        val p = math.max(0.0, math.min(dur, (pt.x.toDouble / size.width) * (maximum - minimum) + minimum))
        setPos(p)
      }

      reactions += {
        case MousePressed(_, pt, mod, _, _) => move(pt, mod)
        case MouseDragged(_, pt, mod)       => move(pt, mod)
      }

      override def paintComponent(g: Graphics2D): Unit = {
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

    def setPos(p: Double): Unit = {
      position = p
      axis.repaint()
      view.repaint()
      lcd.text = timeFormat.format(p, pad = 10)
    }

    // val slider      = new JSlider((view.timeRange._1 * 100).toInt, (view.timeRange._2 * 100).toInt)
    lazy val scroll = new ScrollBar {
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

    def setSpan(start: Double, stop: Double): Unit = {
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
      def apply(): Unit = body
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

    val timer = new javax.swing.Timer(47, ActionListener { _ =>
      val p = playStartPos + (System.currentTimeMillis() - playStartTime) * 0.001
      setPos(p)
    })

    def play(): Unit = {
      stop()
      val sq  = /* if (position == 0) sn else */ {
        val n0  = allNotes.dropWhile(_.offset < position)
        val n1  = n0.map(n => n.copy(offset = n.offset - position))
        val t   = midi.Track(n1.flatMap(_.toMIDI))    // XXX TODO: use different channels for each note/chord group
        midi.Sequence(Vec(t))
      }
      sequencer.play(sq)
      playStartPos  = position
      playStartTime = System.currentTimeMillis()
      timer.restart()
    }

    def stop(): Unit = {
      timer.stop()
      if (sequencer.isPlaying) sequencer.stop()
    }

    def rtz(): Unit = setPos(0.0)

    import Transport._
    val transp = Transport.makeButtonStrip(GoToBegin(rtz()) :: Play(play()) :: Stop(stop()) :: Nil)

    transp.addAction("play-stop", action(plain + VK_SPACE) {
      if (sequencer.isPlaying) stop() else play()
    }, FocusType.Window)

    val box3 = new BoxPanel(Orientation.Vertical) {
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += HGlue
        contents += transp
        contents += HStrut(8)
        contents += lcdP
        contents += HStrut(4)
      }
      contents += box
    }

    rtz()
    lcdP.maximumSize = lcdP.preferredSize // = (200, 20)

    new Frame { frame =>
      contents = new BorderPanel {
        add(Component.wrap(view), BorderPanel.Position.Center)
        add(box3, BorderPanel.Position.North)
        add(box2, BorderPanel.Position.South)
      }
      pack()
      centerOnScreen()
      peer.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)

      new pdflitz.SaveAction(view :: Nil).setupMenu(peer)

      open()
    }
  }
}