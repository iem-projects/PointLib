package at.iem.point.eh.sketches

import de.sciss.midi.{Sequencer, Sequence, Track, TickRate}
import at.iem.point.illism._
import de.sciss.file._

object ContextEvolutions extends App {
  // with start 0, seeds of 1, 2 creates funny loops; 0 and 3 have many walks, 4 is great because it keeps looping but then escapes
  val NUM         = 500 // 2000
  val SEED        = 5L      // seed of the random number generator
  val START       = 0       // start index in the pitch sequence to begin wih
  val VELO        = false   // model velocity
  val VELO_COARSE = 4       // velocity rasterisation (in steps)
  val ENTRY       = true    // model entry offsets
  val ENTRY_COARSE= 0.25    // 0.2     // entry offset rasterisation (relative, in percent 0...1)
  val ENTRY_SCALE = 1.0 // 1.5     // slow down factor if using entry modelling
  val PLAY        = true
  val INTERVALS   = true

  // val sq        = loadSnippet(improvSnippets.last)
  // val notesIn   = sq.notes.sortBy(_.offset)

  val notesIn: Vec[OffsetNote] = {
    // val sqs = (0 to 4).map(loadDisklavier)
    val sqs = loadFirstTests("test-12.mid") :: Nil // +: (1 to 7).map(i => loadFirstTests(s"test-12_1min_$i.mid"))
    val res = ((0.0, Vec.empty[OffsetNote]) /: sqs) { case ((offset, _res), sq) =>
      val ns  = sq.notes
      // assert(ns == ns.sortBy(_.offset))
      val d   = offset - ns.head.offset
      // println(f"Offset $d%1.2f")
      val newNotes  = ns.map(n => n.copy(offset = n.offset + d))
      val newOff    = newNotes.last.stop + newNotes.last.duration
      (newOff, _res ++ newNotes)
    } ._2
    println(s"Total number of notes: ${res.size}")
    res
  }

  val pitchSq   = notesIn.map(_.pitch.midi)
  val sq0 = if (INTERVALS) {
    pitchSq.pairDiff
  } else pitchSq

  implicit val rnd  = new util.Random(SEED)
  val recPch0   = ContextDance.move(sq0 /* pitchSq */, num = NUM)(sq0(START) :: Nil)
  val recPch1 = if (INTERVALS) {
    (Vec(80) /: recPch0)((res, n) => res :+ (res.last + n))
  } else recPch0

  val recPch = recPch1.map(p => math.max(0, math.min(127, p)))

//  println(recPch.map(_.asPitch).mkString(", "))

  val notesOut0 = recPch.zipWithIndex.map { case (midi, idx) =>
    val off = idx * 0.25
    OffsetNote(off, midi.asPitch, 0.125, 80)
  }

  val notesOut1 = if (VELO) {
    val veloSq  = notesIn.map { n => val v = n.velocity; v - (v % VELO_COARSE) }
    val recVelo = ContextDance.move(veloSq, num = NUM)(veloSq(START) :: Nil)
    (notesOut0 zip recVelo).map { case (n, v) => n.copy(velocity = v) }

  } else notesOut0

  implicit val rate = TickRate.tempo(120, 1024)

  //  val sorted = notesIn.sortBy(_.offset)
  //  assert(notesIn == sorted)

  val notesOut  = if (ENTRY) {
    val entrySq = notesIn.sliding(2, 1).to[Vector].map { case Vec(a, b) =>
      val fine    = ((b.offset - a.offset) * 1000 + 0.5).toInt // millis
      val coarse  = (fine * ENTRY_COARSE + 0.5).toInt
      if (coarse > 0) fine - fine % coarse else fine
      //      millis * 1000
    }

    //    println(entrySq.mkString(", "))
    //    ContextDance.DEBUG = true
    val recEntry = ContextDance.move(entrySq :+ entrySq.head, num = NUM)(entrySq(START) :: Nil)
    //    println(recEntry.mkString(", "))
    var off = 0.0
    (notesOut1 zip recEntry).map { case (n, e) =>
      val res = n.copy(offset = off)
      off += e * 0.001 * ENTRY_SCALE
      res
    }

  } else notesOut1

  val events  = notesOut.flatMap(_.toMIDI)
  val track   = Track(events)
  val sqOut   = Sequence(Vector(track))

  // sqOut.writeFile(outPath / s"Improv_PitchSeq_${START}_${SEED}${if (VELO) "V" else ""}${if (ENTRY) "E" else ""}.mid")
  // sqOut.writeFile(outPath / s"Disklavier_PitchSeq_${START}_${SEED}${if (VELO) "V" else ""}${if (ENTRY) "E" else ""}.mid")
  sqOut.writeFile(outPath / s"test-12_PitchSeq_${START}_${SEED}${if (VELO) "V" else ""}${if (ENTRY) "E" else ""}${if (INTERVALS) "I" else ""}.mid")

  if (PLAY) {
    val player = Sequencer.open()
    player.play(sqOut)
    Thread.sleep(((track.duration + 1) * 1000).toLong)
    player.stop()
    player.close()
  }
}