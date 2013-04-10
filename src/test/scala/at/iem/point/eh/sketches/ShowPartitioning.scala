package at.iem.point.eh.sketches

import java.awt.{BorderLayout, EventQueue}
import at.iem.point.illism._
import at.iem.point.illism.gui.PianoRoll
import javax.swing.{JScrollBar, Box, JSlider, SwingConstants, WindowConstants, JFrame}
import de.sciss.audiowidgets.j.Axis

object ShowPartitioning extends App with Runnable{
  EventQueue.invokeLater(this)

  def run() {
    val sn          = loadSnippet(improvSnippets(0))
    val (m, h)      = NoteUtil.splitMelodicHarmonic(sn.notes)
    val nm          = m.flatMap(_._2)
    val nh          = h.flatMap(_._2)
    val view        = PianoRoll.j()
    view.notes      = nm
    view.chords     = nh
    view.timeRange  = (0.0, 20.0)
    // val deco        = PianoRoll.NoteDecoration(Some(Color.red))
    // view.decoration = nh.map(n => n -> deco)(breakOut)
    val axis        = new Axis(SwingConstants.HORIZONTAL)
    axis.format     = Axis.Format.Time(hours = false, millis = true)
    axis.minimum    = view.timeRange._1
    axis.maximum    = view.timeRange._2
    // val slider      = new JSlider((view.timeRange._1 * 100).toInt, (view.timeRange._2 * 100).toInt)
    val scroll      = new JScrollBar(SwingConstants.HORIZONTAL, 0, 10, 0, 100)  // XXX TODO
    val box         = Box.createHorizontalBox()
    box.add(Box.createHorizontalStrut(view.keyWidth))
    box.add(axis)

    new JFrame {
      val cp = getContentPane
      cp.add(view,   BorderLayout.CENTER)
      cp.add(box,    BorderLayout.NORTH)
      cp.add(scroll, BorderLayout.SOUTH)
      pack()
      setLocationRelativeTo(null)
      setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
      setVisible(true)
    }
  }
}