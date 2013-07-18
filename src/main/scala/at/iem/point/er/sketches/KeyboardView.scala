package at.iem.point.er.sketches

import scala.swing.{Swing, Component}
import java.awt.{Insets, Graphics2D}
import de.sciss.swingplus.Implicits._
import Swing._

class KeyboardView(model0: Keyboard = new Keyboard) extends Component {
  private var _keyboard = model0
  recalcPref()

  private val listener: Keyboard.Listener = {
    case Keyboard.ColorsChanged(_) => repaint()
    case Keyboard.PitchRangeChanged(_) | Keyboard.KeySizeChanged(_) =>
      recalcPref()
      repaint()
  }

  def model = _keyboard
  def model_=(kb: Keyboard) {
    _keyboard.removeListener(listener)
    _keyboard = model
    _keyboard.addListener(listener)
    repaint()
  }

  _keyboard.addListener(listener)

  private def recalcPref(): Unit = {
    val kw  = _keyboard.keyWidth
    val kh0 = _keyboard.keyHeight
    val tup = _keyboard.pitchRange
    val sz  = tup._2 - tup._1
    val kh  = (if (kh0 == -1) 8 else kh0) * sz
    preferredSize = (kw, kh)
  }

  private val in = new Insets(0, 0, 0, 0)

  override protected def paintComponent(g: Graphics2D): Unit = {
    peer.getInsets(in)
    // println(in)
    _keyboard.paint(g, in.left, in.top, this.width - (in.left + in.right), this.height - (in.top + in.bottom))
  }
}