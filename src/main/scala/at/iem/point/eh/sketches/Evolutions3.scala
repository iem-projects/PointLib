package at.iem.point.eh.sketches

import at.iem.point.illism._
import de.sciss.midi.{Sequencer, Sequence, Track, TickRate}

object Evolutions3 extends App {
  // with start 0, seeds of 1, 2 creates funny loops; 0 and 3 have many walks, 4 is great because it keeps looping but then escapes
  val NUM         = 200
  val SEED        = 10L     // seed of the random number generator
  val START       = 10      // start index in the pitch sequence to begin wih
  val VELO        = false   // model velocity
  val VELO_COARSE = 4       // velocity rasterisation (in steps)
  val ENTRY       = true    // model entry offsets
  val ENTRY_COARSE= 0.2     // entry offset rasterisation (relative, in percent 0...1)
  val ENTRY_SCALE = 1.5     // slow down factor if using entry modelling

  val INNER       = true   // model inner structure of chords (`true`) or just frame intervals (`false`)

  val sq        = loadSnippet(improvSnippets.last)
  val notesIn0  = sq.notes
  val (_, h)    = NoteUtil.splitMelodicHarmonic(notesIn0)
  val chordsIn  = h.flatMap(_._2)

  val pchSq     = chordsIn.map { c => val pch = c.pitches; pch.head.`class`.step -> pch.last.`class`.step }
  val recPch    = ContextDance.move(pchSq, num = NUM, seed = SEED)(pchSq(START) :: Nil)
  val octSq     = chordsIn.map { c => val pch = c.pitches; (pch.head.midi / 12) -> (pch.last.midi / 12) }
  val recOct    = ContextDance.move(octSq, num = NUM, seed = SEED)(octSq(START) :: Nil)
  val voiceSq   = chordsIn.map(_.size)
  val recVoice  = ContextDance.move(voiceSq, num = NUM, seed = SEED)(voiceSq(START) :: Nil)
//  val maxVoices = voiceSq.max
//  val maxFrame  = chordsIn.map(_.frameInterval.semitones).max
  implicit val rnd = new util.Random(SEED)

  // a simple occurrence map
  var inner     = Map.empty[Int, Map[Int, IIdxSeq[Chord]]] withDefaultValue (Map.empty withDefaultValue Vector.empty)
  chordsIn.foreach { c =>
    val semi  = c.frameInterval.semitones
    val map0  = inner(semi)
    val map   = map0 + (c.size -> (map0(c.size) :+ c))
    inner += semi -> map
  }

  val chordsOut0 = ((recPch zip recOct) zip recVoice).zipWithIndex.map {
    case ((((classLo, classHi), (octLo, octHi)), voices), idx) =>

    val off     = idx * 0.25
    val pchLo0  = classLo + octLo * 12
    val pchHi0  = classHi + octHi * 12
    val pchLo   = math.min(pchLo0, pchHi0)
    val pchHi   = math.max(pchLo0, pchHi0)
    val noteLo  = OffsetNote(off, pchLo.asPitch, 0.125, 80)

//    println(s"Note $pchLo $pchHi $voices")

    if (INNER) {
      var semi  = pchHi - pchLo
      assert(semi >= 0, semi)  // NOT TRUE
      var semiInc = -1
      while (inner(semi).isEmpty) {
        if (semi == 0) semiInc = 1
        semi += semiInc
      }
      val map = inner(semi)
//      assert(map.nonEmpty)
      var vc    = voices
      assert(vc > 1)
      var vcInc = -1
      while (map(vc).isEmpty) {
        if (vc == 1) vcInc = 1
        vc += vcInc
      }
//      println("---1")
      val chord   = map(vc).choose
      val ivals   = chord.layeredIntervals.map(_.semitones).scramble
      assert(ivals.sum == semi)
      val chords  = ivals.integrate.map { step =>
        OffsetNote(off, (pchLo + step).asPitch, 0.125, 80)
      }
      Chord(noteLo +: chords)

    } else {
      val noteHi = OffsetNote(off, pchHi.asPitch, 0.125, 80)
      Chord(Vector(noteLo, noteHi))
    }
  }

  val chordsOut1 =
//    if (VELO) {
//    val veloSq  = notesIn.map { n => val v = n.velocity; v - (v % VELO_COARSE) }
//    val recVelo = ContextDance.move(veloSq, num = NUM, seed = SEED)(veloSq(START) :: Nil)
//    (notesOut0 zip recVelo).map { case (n, v) => n.copy(velocity = v) }
//
//  } else
    chordsOut0

  implicit val rate = TickRate.tempo(120, 1024)

  val chordsOut  =
    if (ENTRY) {
    val entrySq = chordsIn.sliding(2, 1).to[Vector].map { case Seq(a, b) =>
      val fine    = ((b.avgOffset - a.avgOffset) * 1000 + 0.5).toInt // millis
      val coarse  = (fine * ENTRY_COARSE + 0.5).toInt
      if (coarse > 0) (fine - fine % coarse) else fine
//      millis * 1000
    }

//    println(entrySq.mkString(", "))
//    ContextDance.DEBUG = true
    val recEntry = ContextDance.move(entrySq :+ entrySq.head, num = NUM, seed = SEED)(entrySq(START) :: Nil)
//    println(recEntry.mkString(", "))
    var off = 0.0
    (chordsOut1 zip recEntry).map { case (c, e) =>
      val delta = off - c.avgOffset
      val res = Chord(c.notes.map(n => n.copy(offset = n.offset + delta)))
      off += e * 0.001 * ENTRY_SCALE
      res
    }

  } else
    chordsOut1

  val events  = chordsOut.flatMap(_.toMIDI)
  val track   = Track(events)
  val sqOut   = Sequence(Vector(track))

  sqOut.writeFile(outPath / s"Improv_ChordSeq_${START}_${SEED}${if (INNER) "I" else ""}${if (VELO) "V" else ""}${if (ENTRY) "E" else ""}.mid")

  val player  = Sequencer.open()
  player.play(sqOut)
  Thread.sleep(((track.duration + 1) * 1000).toLong)
  player.stop()
  player.close()
}