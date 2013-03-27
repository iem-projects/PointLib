package at.iem.point.eh.sketches

import at.iem.point.illism._
import de.sciss.midi.{Sequencer, Sequence, Track, TickRate}

object Evolutions3 extends App {
  // with start 0, seeds of 1, 2 creates funny loops; 0 and 3 have many walks, 4 is great because it keeps looping but then escapes
  val NUM         = 200
  val SEED        = 3L      // seed of the random number generator
  val START       = 0       // start index in the pitch sequence to begin wih
  val VELO        = false    // model velocity
  val VELO_COARSE = 4       // velocity rasterisation (in steps)
  val ENTRY       = false    // model entry offsets
  val ENTRY_COARSE= 0.2     // entry offset rasterisation (relative, in percent 0...1)
  val ENTRY_SCALE = 1.5     // slow down factor if using entry modelling

  val INNER       = false    // model inner structure of chords (`true`) or just frame intervals (`false`)

  val sq        = loadSnippet(improvSnippets.last)
  val notesIn0  = sq.notes
  val (_, h)    = NoteUtil.splitMelodicHarmonic(notesIn0)
  val chordsIn  = h.flatMap(_._2)

  val pchSq     = chordsIn.map { c => val pch = c.pitches; pch.head.`class`.step -> pch.last.`class`.step }
  val recPch    = ContextDance.move(pchSq, num = NUM, seed = SEED)(pchSq(START) :: Nil)
  val octSq     = chordsIn.map { c => val pch = c.pitches; (pch.head.midi / 12) -> (pch.last.midi / 12) }

  val notesOut0 = (pchSq zip octSq).zipWithIndex.flatMap { case (((classLo, classHi), (octLo, octHi)), idx) =>
    val off     = idx * 0.25
    val pchLo   = classLo + octLo * 12
    val pchHi   = classHi + octHi * 12
    val noteLo  = OffsetNote(off, pchLo.asPitch, 0.125, 80)
    val noteHi  = OffsetNote(off, pchHi.asPitch, 0.125, 80)
    if (INNER) {

    }
    noteLo :: noteHi :: Nil
  }

  val notesOut1 =
//    if (VELO) {
//    val veloSq  = notesIn.map { n => val v = n.velocity; v - (v % VELO_COARSE) }
//    val recVelo = ContextDance.move(veloSq, num = NUM, seed = SEED)(veloSq(START) :: Nil)
//    (notesOut0 zip recVelo).map { case (n, v) => n.copy(velocity = v) }
//
//  } else
      notesOut0

  implicit val rate = TickRate.tempo(120, 1024)

  val notesOut  =
//    if (ENTRY) {
//    val entrySq = notesIn.sliding(2, 1).to[Vector].map { case Seq(a, b) =>
//      val fine    = ((b.offset - a.offset) * 1000 + 0.5).toInt // millis
//      val coarse  = (fine * ENTRY_COARSE + 0.5).toInt
//      if (coarse > 0) (fine - fine % coarse) else fine
////      millis * 1000
//    }
//
////    println(entrySq.mkString(", "))
////    ContextDance.DEBUG = true
//    val recEntry = ContextDance.move(entrySq :+ entrySq.head, num = NUM, seed = SEED)(entrySq(START) :: Nil)
////    println(recEntry.mkString(", "))
//    var off = 0.0
//    (notesOut1 zip recEntry).map { case (n, e) =>
//      val res = n.copy(offset = off)
//      off += e * 0.001 * ENTRY_SCALE
//      res
//    }
//
//  } else
    notesOut1

  val events  = notesOut.flatMap(_.toMIDI)
  val track   = Track(events)
  val sqOut   = Sequence(Vector(track))

  sqOut.writeFile(outPath / s"Improv_ChordSeq_${START}_${SEED}${if (INNER) "I" else ""}${if (VELO) "V" else ""}${if (ENTRY) "E" else ""}.mid")

  val player  = Sequencer.open()
  player.play(sqOut)
  Thread.sleep(((track.duration + 1) * 1000).toLong)
  player.stop()
  player.close()
}