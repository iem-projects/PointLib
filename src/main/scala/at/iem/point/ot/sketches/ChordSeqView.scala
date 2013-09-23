package at.iem.point.ot.sketches

import at.iem.point.illism._
import scala.swing.{Swing, Component}
import java.awt.{Color, Graphics2D}
import abc.ui.swing.JScoreComponent
import Swing._
import abc.notation.{Note => _Note, MusicElement, Accidental, MultiNote, BarLine, Spacer, Clef, Tune, KeySignature}
import scala.collection.mutable
import scala.swing.event.{ValueChanged, Key, MousePressed}

class ChordSeqView extends Component {
  private var _chords     = Vec.empty[(Chord, ChordEval)]
  private var dirty       = true
  // private var noteMap     = Map.empty[_Note, Int]
  private var chordElems  = Vec.empty[MultiNote]

  // rendering is faster if we'd cache JScoreComponent instances,
  // however that produces glitches somehow (spacing is different).
  // my guess is that sucky library is not fully reentrant.
  // therefore, only caches tunes now, still gives some speed improvement.
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
  private val cache = mutable.WeakHashMap.empty[Vec[(Chord, ChordEval)], (Tune, Vec[MultiNote])]

  preferredSize = (400, 100)

  listenTo(mouse.clicks)
  reactions += {
    case MousePressed(_, pt, mod, cnt, _) =>
      //      pt.y -= 56
      //      val seo = Option(scoreView.getScoreElementAt(pt))
      //      println(s"at $pt have $seo")
      //      seo.map(_.getMusicElement).foreach {
      //        case n: _Note =>
      //          println(n)
      //        case _ =>
      //      }

      val x = pt.x
      val chordIdx = chordElems.indexWhere { mn =>
        val r = scoreView.getRenditionElementFor(mn).getBoundingBox
        r.getMinX <= x && r.getMaxX + 4 > x   // reported right margin is too small!
      }

      // println(s"Chord index $chordIdx")
      if (chordIdx >= 0) {
        val isAlt = (mod & Key.Modifier.Alt) != 0
        if (cnt == 2 || isAlt) {
          val (c, oldEval) = _chords(chordIdx)
          val toggle  = if (isAlt) ChordBad else ChordGood
          val newEval = if (oldEval == toggle) ChordNeutral else toggle
          chords = _chords.updated(chordIdx, (c, newEval))
          // println(s"publish! $chords")
          publish(new ValueChanged(this))
        }
      }
  }

  def chords: Vec[(Chord, ChordEval)] = _chords
  def chords_=(value: Vec[(Chord, ChordEval)]): Unit = {
    require(value != null)
    _chords = value
    dirty = true
  }

  override protected def paintComponent(g: Graphics2D): Unit = {
    if (dirty) rebuild()

    g.setColor(Color.white)
    g.fillRect(0, 0, peer.getWidth, peer.getHeight)

    // g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL   , RenderingHints.VALUE_STROKE_PURE         )
    // g.setRenderingHint(RenderingHints.KEY_FRACTIONALMETRICS, RenderingHints.VALUE_FRACTIONALMETRICS_ON)
    val atOrig = g.getTransform
    try {
      g.translate(0,  56)
      // val t1 = System.currentTimeMillis()
      scoreView.drawIn(g)
      // val t2 = System.currentTimeMillis()
      // println(s"draw ${t2-t1}")
      g.translate(0,  36)
      bassView.drawIn(g)
    } finally {
      g.setTransform(atOrig)
    }
  }

  private def rebuild(): Unit = {
    val (tune, map) = cache.get(_chords).getOrElse {
      val res = rebuildReally()
      cache.put(_chords, res)
      res
    }
    scoreView.setTune(tune)
    (_chords zip map).foreach { case ((c, e), mn) =>
      if (e == ChordGood || e == ChordBad) {
        val colr  = if (e == ChordGood) Color.blue else Color.red
        mn.toArray.foreach { n =>
          val r = scoreView.getRenditionElementFor(n)
          // println(s"r $r -> $e")
          r.setColor(colr)
        }
      }
    }
    chordElems = map
    dirty   = false
  }

  //  private val parser      = new abc.parser.TuneParser
  //  private val pitchNames  = " C^C D^D E F^F G^G A^A B"
  //  private def rebuildReallyPARSER(): Tune = {
  //    /*
  //
  //    On the ABC format:
  //    ( cf. http://abcnotation.com/wiki/abc:standard:v2.1 )
  //
  //    - needs K:C to make key (C for simplicity
  //    - pitch names C D E F G A B C
  //    - C  is middle C (C4), 261. Hz, MIDI 60
  //    - C, is C3, C' is C5, etc.; C' = c, C'' = c', etc.
  //    - _C is Ces, ^C is Cis, __C is Ceses, ^^ is Cisis, etc.
  //    - Z for rest
  //    - default duration is 1/8. C2 is middle C 1/4, C4 is middle C 1/2, etc. Explicit: L:1/8
  //    - | (pipe) is vertical bar line
  //
  //    [...] to make chords
  //
  //     */
  //
  //    val abc = _chords.map { case (c, _) =>
  //      c.pitches.map { p =>
  //        val m   = p.midi
  //        val clz = m % 12
  //        val oct = m / 12
  //        val i   = clz << 1
  //        val pn   = pitchNames.substring(if (pitchNames.charAt(i) == ' ') i + 1 else i, i + 2)
  //        val name = if (oct > 5) {
  //          pn + ("'" * (oct - 5))
  //        } else if (oct < 5) {
  //          pn + ("," * (5 - oct))
  //        } else pn
  //        name + "8"  // whole notes
  //      } .mkString("[", "", "]")
  //    } .mkString("|")
  //
  //    //    val tune    = new Tune()
  //    //    val music   = tune.getMusic
  //    //
  //    //    val keyT    = new KeySignature(_Note.C, KeySignature.MAJOR)
  //    //    // val keyB    = new KeySignature(_Note.C, KeySignature.MAJOR)
  //    //    // keyB.setClef(Clef.BASS)
  //    //    val voiceT  = music.getVoice("1")
  //    //    val voiceB  = music.getVoice("2")
  //    //    voiceT.addElement(keyT)
  //    //    voiceB.addElement(Clef.BASS) // keyB)
  //    //    val n1 = new _Note(_Note.C)
  //    //    n1.setStrictDuration(_Note.QUARTER)
  //    //    voiceT.addElement(n1)
  //    //    // voiceB.setFirstBarNumber(0)
  //    //    val r1 = new _Note(_Note.REST)
  //    //    r1.setStrictDuration(_Note.QUARTER)
  //    //    voiceB.addElement(r1)
  //
  //    //    val test =
  //    //      """|X:1
  //    //         |T:Fugue in Cm
  //    //         |C:J.S.Bach
  //    //         |V:1 %program 1 19
  //    //         |V:2 %program 1 19
  //    //         |V:3 bass %program 1 19
  //    //         |M:4/4
  //    //         |Q:100
  //    //         |K:Cm
  //    //         |[V:1]  z8   |  z8     | zg/^f/g ce g/f/g=a   |
  //    //         |[V:2] z c/=B/c GA c/B/cd | G c/=B/c dF/G/A2G/F/ | E/c/=B/=A/G/F/ E/D/ Cedc |
  //    //         |[V:3]  z8     |  z8  |  z8  |
  //    //         |""".stripMargin
  //
  //    // val t1 = System.currentTimeMillis()
  //    val tune = parser.parse("K:C\n|" + abc)
  //    // val t2 = System.currentTimeMillis()
  //    // val _scoreView = new JScoreComponent
  //    // scoreView.setTune(tune)
  //    // val t3 = System.currentTimeMillis()
  //    // println(s"parse ${t2-t1}; set ${t3-t2}")
  //    tune
  //  }

  private def rebuildReally(): (Tune, Vec[MultiNote]) = {
    val tune  = new Tune()
    val m     = tune.getMusic
    val v     = m.getFirstVoice
    val ks    = new KeySignature(_Note.C, KeySignature.MAJOR)
    v.addElement(ks)

    // var map   = Map.empty[_Note, Int] withDefaultValue -1

    val mns = _chords.map { case (c, _) =>
      val ns = new java.util.Vector[_Note](c.size)
      c.pitches.foreach { p =>
        val midi  = p.midi
        val clz   = midi % 12
        val oct   = midi / 12
        val (h, a) = clz match {
          case 0 | 2 | 4 | 5 | 7 | 9 | 11 => (clz    , Accidental.NONE)
          case _                          => (clz - 1, Accidental.SHARP)
        }

        val n = new _Note(h.toByte, a, (oct - 5).toByte)  // _Note.createFromMidiLikeHeight(60 - 60)
        n.setStrictDuration(_Note.WHOLE)
        ns.add(n)
        // map += n -> idx
      }
      val mn = new MultiNote(ns)
      v.addElement(mn)
      v.addElement(new BarLine())

      mn
    }

    (tune, mns)
  }
}