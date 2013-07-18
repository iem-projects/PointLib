package at.iem.point.er.sketches

import java.awt.{Font, Color, RenderingHints, Graphics2D}
import de.sciss.model.impl.ModelImpl
import java.awt.geom.RoundRectangle2D
import de.sciss.model.Model
import javax.swing.UIManager
import java.util.Locale

object Keyboard {
  sealed trait Update { def keyboard: Keyboard }
  case class KeySizeChanged   (keyboard: Keyboard) extends Update
  case class PitchRangeChanged(keyboard: Keyboard) extends Update
  case class ColorsChanged    (keyboard: Keyboard) extends Update

  type Listener = Model.Listener[Update]
}
/** A piano keyboard painter. */
class Keyboard extends ModelImpl[Keyboard.Update] {
  import Keyboard._

  private var _pitchRange     = (21, 109)   // defaultPitchRange
  private var _keyWidth       = 48          // defaultKeyWidth
  private var _keyHeight      = 8           // defaultKeyHeight
  private var _background     = Color.black
  private var _whiteKeysColor = Color.white
  private var _blackKeysColor = Color.black
  private var _cKeysColor     = Color.lightGray
  private var _foreground     = Color.black

  private def isBlack(pch: Int) = {
    val c = pch % 12
    c == 1 || c == 3 || c == 6 || c == 8 || c == 10
  }

  /** The pitch range given as an interval `[start, stop)` of midi values */
  def pitchRange = _pitchRange
  def pitchRange_=(value: (Int, Int)): Unit = {
    if (_pitchRange != value) {
      _pitchRange = value
      dispatch(PitchRangeChanged(this))
    }
  }

  /** The pixel width of the keys */
  def keyWidth = _keyWidth
  def keyWidth_=(value: Int): Unit = {
    if (_keyWidth != value) {
      _keyWidth = value
      dispatch(PitchRangeChanged(this))
    }
  }

  /** The nominal pixel height of the keys. Special meaning is given to the value `-1`
    * which means that the keys should automatically size with the painting area given.
    */
  def keyHeight = _keyHeight
  /** The nominal pixel height of the keys. Special meaning is given to the value `-1`
    * which means that the keys should automatically size with the painting area given.
    */
  def keyHeight_=(value: Int): Unit = {
    val old = _keyHeight
    if (value == -1) {
      _keyHeight = value
    } else {
      val even = value & ~1
      _keyHeight = even
    }

    if (old != _keyHeight) {
      dispatch(PitchRangeChanged(this))
    }
  }

  /** The background color */
  def background = _background
  def background_=(value: Color): Unit = {
    if (_background != value) {
      _background = value
      dispatch(ColorsChanged(this))
    }
  }

  /** The foreground color */
  def foreground = _foreground
  def foreground_=(value: Color): Unit = {
    if (_foreground != value) {
      _foreground = value
      dispatch(ColorsChanged(this))
    }
  }

  /** The color of the white keys */
  def whiteKeysColor = _whiteKeysColor
  def whiteKeysColor_=(value: Color): Unit = {
    if (_whiteKeysColor != value) {
      _whiteKeysColor = value
      dispatch(ColorsChanged(this))
    }
  }

  /** The color of the black keys */
  def blackKeysColor = _blackKeysColor
  def blackKeysColor_=(value: Color) {
    if (_blackKeysColor != value) {
      _blackKeysColor = value
      dispatch(ColorsChanged(this))
    }
  }

  /** The color of the `C` keys. */
  def cKeysColor = _cKeysColor
  def cKeysColor_=(value: Color): Unit = {
    if (_cKeysColor != value) {
      _cKeysColor = value
      dispatch(ColorsChanged(this))
    }
  }

  private val rr = new RoundRectangle2D.Double

  /** Paints the keyboard into a given graphics context, using a top-left pixel offset and
    * painting area size. The area (`w`, `h`) is only used as a clipping rectangle, whereas
    * key sizes and positions are determined by `keyWidth` and `keyHeight`, beginning at
    * the bottom left of the painting area. When `keyHeight` is `-1`, painting will use
    * automatic vertical scaling.
    */
  def paint(g: Graphics2D, x: Int, y: Int, w: Int, h: Int): Unit = {
    val clipOrig = g.getClip

    val (start, stop) = _pitchRange
    val kh            = if (_keyHeight == -1) {
      // only approximative, assuming we go from white keys to white keys:
      // octave has 4 * 1.5 kh + 3 * 2.0 kh. thus one octave takes 12 * kh
      h.toDouble / (stop - start)
    } else _keyHeight
    val _keySize1     = kh * 1.5
    val _keySize2     = kh * 2

    //println(s"_keyHeight = ${_keyHeight}; kh = $kh")

    val font = {
      val f0  = UIManager.getFont("Slider.font", Locale.US)
      val f   = if (f0 == null) new Font("SansSerif", Font.PLAIN, 9) else f0
      val factor = kh.toFloat / 6
      f.deriveFont(8.5f * factor)
    }
    g.setFont(font)

    try {
      g.clipRect(x, y, w, h)
      g.setRenderingHint(RenderingHints.KEY_ANTIALIASING  , RenderingHints.VALUE_ANTIALIAS_ON)
      g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE )
      val bw = _keyWidth * 2 / 3
      g.setColor(_background)
      g.fillRect(x, y, w, h)

      var iw  = start
      var y0  = y + h - kh
      while (iw < stop) {
        if (!isBlack(iw)) {
          val c       = iw % 12
          val isWide  = c == 2 || c == 7 || c == 9
          val sz      = if (isWide) _keySize2 else _keySize1
          val shift   = if (c == 4 || c == 11) 0 else if (c == 0 || c == 5) sz - kh else (sz - kh) / 2
          g.setColor(if (c == 0) _cKeysColor else _whiteKeysColor)
          // g.fillRoundRect(x, y0 - shift, w - 1, sz - 1, 4, 4)
          rr.setRoundRect(x, y0 - shift, w - 1, sz - 1, 4, 4)
          g.fill(rr)
          if (c == 0) {
            g.setColor(_foreground)
            val fm  = g.getFontMetrics
            val str = (iw / 12 - 1).toString
            val sw  = fm.stringWidth(str)
            g.drawString(str, x + w - 4 - sw, (y0 - shift).toFloat + fm.getAscent - fm.getDescent + 0.5f)
          }
        }
        y0 -= kh
        iw += 1
      }

      y0      = y + h - kh
      var ib  = start
      g.setColor(_blackKeysColor)
      // val _ks1h = kh >> 1
      while (ib < stop) {
        if (isBlack(ib)) {
          // g.fillRoundRect(x - 1, y0, bw, kh, 4, 4)
          rr.setRoundRect(x - 1, y0, bw, kh, 4, 4)
          g.fill(rr)
        }
        y0 -= kh
        ib += 1
      }

    } finally {
      g.setClip(clipOrig)
    }
  }
}