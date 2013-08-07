package at.iem.point.ot.sketches

import at.iem.point.illism._
import scala.swing.{Swing, Component}
import java.awt.Graphics2D
import abc.ui.swing.JScoreComponent
import abc.parser.TuneParser
import Swing._

class ChordSeqView extends Component {
  private var _chords   = Vec.empty[Chord]
  private var dirty     = true
  private val scoreView = new JScoreComponent
  private val parser    = new TuneParser

  preferredSize = (400, 100)

  def chords: Vec[Chord] = _chords
  def chords_=(value: Vec[Chord]): Unit = {
    _chords = value
    dirty = true
  }

  override protected def paintComponent(g: Graphics2D): Unit = {
    if (dirty) rebuild()
    scoreView.drawIn(g)
  }

  private def rebuild(): Unit = {
//    val tune  = new Tune
//    val part  = tune.createPart("part")

    val tune = parser.parse("K:C\n|[C2A2]")
    scoreView.setTune(tune)
    dirty = false
  }
}