package at.iem.point.ot.sketches

import scala.swing.{Swing, Component, MainFrame, SimpleSwingApplication}
import abc.ui.swing.JScoreComponent
import Swing._
import abc.notation.{BarLine, Note, KeySignature, Tune}
import java.awt.Color
import scala.swing.event.MouseMoved

object ChordViewTest extends SimpleSwingApplication {
  lazy val top = new MainFrame {
    val scoreView = new JScoreComponent

    val tune = new Tune()
    // tune.addTitle("Foo")
    // tune.getKey.setClef(Clef.TENOR())
    val m = tune.getMusic
    val v = m.getFirstVoice
    // println(m.getVoices.mkString(", "))
    val ks = new KeySignature(Note.C, KeySignature.MAJOR)
    // v.addElement(Clef.TENOR())
    v.addElement(ks)
    val elem = new Note(Note.C)
    v.addElement(elem)
    v.addElement(new BarLine())
    v.addElement(new Note(Note.E))
    scoreView.setTune(tune)

    val comp = Component.wrap(scoreView)
    // comp.opaque = false

    contents  = comp
    size      = (400, 400)

    listenTo(comp.mouse.moves)

    var highlight = Option.empty[Note]

    reactions += {
      case MouseMoved(_, pt, _) =>
        Option(scoreView.getScoreElementAt(pt)).flatMap(r => Option(r.getMusicElement)).foreach {
          case nt: Note =>
            // println(s"Note! $nt")
            highlight.foreach { n1 =>
              val r = scoreView.getRenditionElementFor(n1)
              r.setColor(null)
            }
            highlight = Some(nt)
            val r = scoreView.getRenditionElementFor(nt)
            r.setColor(Color.red)
            scoreView.repaint()

          case other => // println(s"Found $other")
        }
    }
  }
}
