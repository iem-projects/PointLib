package at.iem.point.ot.sketches

import at.iem.point.illism._
import scala.swing.{Swing, Component}
import java.awt.{RenderingHints, Graphics2D}
import abc.ui.swing.JScoreComponent
import abc.parser.TuneParser
import Swing._
import abc.notation.{Note => _Note, Spacer, Clef, Tune, KeySignature}

class ChordSeqView extends Component {
  private var _chords     = Vec.empty[Chord]
  private var dirty       = true
  private val scoreView   = new JScoreComponent
  private val bassView    = {
    val res = new JScoreComponent
    val tun = new Tune
    val mus = tun.getMusic
    val voc = mus.getFirstVoice
    val key = new KeySignature(_Note.C, KeySignature.MAJOR)
    key.setClef(Clef.BASS)
    voc.addElement(key)
    // val n1 = new _Note(_Note.C)
    // n1.setStrictDuration(_Note.QUARTER)
    val n1 = new Spacer(100f)
    voc.addElement(n1)
    res.setTune(tun)
    res
  }
  private val parser      = new TuneParser

  private val pitchNames  = " C^C D^D E F^F G^G A^A B"

  preferredSize = (400, 100)

  def chords: Vec[Chord] = _chords
  def chords_=(value: Vec[Chord]): Unit = {
    _chords = value
    dirty = true
  }

  override protected def paintComponent(g: Graphics2D): Unit = {
    if (dirty) rebuild()

    // g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL   , RenderingHints.VALUE_STROKE_PURE         )
    g.setRenderingHint(RenderingHints.KEY_FRACTIONALMETRICS, RenderingHints.VALUE_FRACTIONALMETRICS_ON)
    val atOrig = g.getTransform
    try {
      g.translate(0,  56)
      scoreView.drawIn(g)
      // g.translate(0,  39)
      // bassView.drawIn(g)
    } finally {
      g.setTransform(atOrig)
    }
  }

  private def rebuild(): Unit = {
    /*

    On the ABC format:
    ( cf. http://abcnotation.com/wiki/abc:standard:v2.1 )

    - needs K:C to make key (C for simplicity
    - pitch names C D E F G A B C
    - C  is middle C (C4), 261. Hz, MIDI 60
    - C, is C3, C' is C5, etc.; C' = c, C'' = c', etc.
    - _C is Ces, ^C is Cis, __C is Ceses, ^^ is Cisis, etc.
    - Z for rest
    - default duration is 1/8. C2 is middle C 1/4, C4 is middle C 1/2, etc. Explicit: L:1/8
    - | (pipe) is vertical bar line

    [...] to make chords

     */

    val abc = _chords.map { c =>
      c.pitches.map { p =>
        val m   = p.midi
        val clz = m % 12
        val oct = m / 12
        val i   = clz << 1
        val pn   = pitchNames.substring(if (pitchNames.charAt(i) == ' ') i + 1 else i, i + 2)
        val name = if (oct > 5) {
          pn + ("'" * (oct - 5))
        } else if (oct < 5) {
          pn + ("," * (5 - oct))
        } else pn
        name + "8"  // whole notes
      } .mkString("[", "", "]")
    } .mkString("|")

    //    val tune    = new Tune()
    //    val music   = tune.getMusic
    //
    //    val keyT    = new KeySignature(_Note.C, KeySignature.MAJOR)
    //    // val keyB    = new KeySignature(_Note.C, KeySignature.MAJOR)
    //    // keyB.setClef(Clef.BASS)
    //    val voiceT  = music.getVoice("1")
    //    val voiceB  = music.getVoice("2")
    //    voiceT.addElement(keyT)
    //    voiceB.addElement(Clef.BASS) // keyB)
    //    val n1 = new _Note(_Note.C)
    //    n1.setStrictDuration(_Note.QUARTER)
    //    voiceT.addElement(n1)
    //    // voiceB.setFirstBarNumber(0)
    //    val r1 = new _Note(_Note.REST)
    //    r1.setStrictDuration(_Note.QUARTER)
    //    voiceB.addElement(r1)

    val tune = parser.parse("K:C\n|" + abc)
    scoreView.setTune(tune)
    dirty = false
  }
}