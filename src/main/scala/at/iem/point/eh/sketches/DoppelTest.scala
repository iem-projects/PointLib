package at.iem.point.eh.sketches

import at.iem.point.illism._
import de.sciss.midi.{Sequencer, Sequence, Track, TickRate}
import de.sciss.file._

object DoppelTest extends App {
  // with start 0, seeds of 1, 2 creates funny loops; 0 and 3 have many walks, 4 is great because it keeps looping but then escapes
  val NUM         = 1000
  val SEED        = 332L    // seed of the random number generator
  val START       = 1       // start index in the pitch sequence to begin with
  val VELO        = true    // true    // model velocity
  val VELO_COARSE = 6       // velocity rasterisation (in steps)
  val ENTRY_COARSE= 2.0     // entry offset rasterisation
  val INTERVALS   = true
  val ACCEL       = false   // true
  val PLAY        = true

//  val sq        = loadSnippet(improvSnippets.last)
//  val notesIn0  = sq.notes
//  val (m, _)    = NoteUtil.splitMelodicHarmonic(notesIn0)
//  val mm        = m.map(_._2)

  // val mm = Vec(loadFirstTests("test-12.mid").notes)
  // val mm = ( /* loadFirstTests("test-12.mid") +: */ (1 to 5).map(i => loadFirstTests(s"test-12_1min_$i.mid"))).map(_.notes)

  //  val mm: Vec[Vec[OffsetNote]] = {
  //    // val sqs = (0 to 4).map(loadDisklavier)
  //    val sqs = (1 to 5).map(i => loadFirstTests(s"test-12_1min_$i.mid"))
  //    val res = ((0.0, Vec.empty[OffsetNote]) /: sqs) { case ((offset, _res), sq) =>
  //        val ns  = sq.notes
  //        // assert(ns == ns.sortBy(_.offset))
  //        val d   = offset - ns.head.offset
  //        // println(f"Offset $d%1.2f")
  //        val newNotes  = ns.map(n => n.copy(offset = n.offset + d))
  //        val newOff    = newNotes.last.stop + newNotes.last.duration
  //        (newOff, _res ++ newNotes)
  //      } ._2
  //    println(s"Total number of notes: ${res.size}")
  //    Vec(res)
  //  }

  val sn3i  = Vec(42)
  val sn3   = sn3i.map(loadSnippet)
  val name  = s"Snippet${sn3i.mkString("_")}"
  val ENTRY_SCALE = 1.25
  val snSel = sn3
  val notesIn0 = joinSnippets(snSel)
  val notesIn = notesIn0 :+ {
    val h = notesIn0.head
    val l = notesIn0.last
    h.copy(offset = l.stop)
  }

  implicit val rnd  = new util.Random(SEED)
  val gen       = DoppelDance(startIdx = START, modelVelo = VELO, veloCoarse = VELO_COARSE,
  entryCoarse = ENTRY_COARSE, entryScale = ENTRY_SCALE, intervals = INTERVALS, accel = ACCEL, notesIn = notesIn)
  val notesOut  = gen.move(NUM)

  implicit val rate = TickRate.tempo(120, 1024)
  val events  = notesOut.flatMap(_.toMIDI)
  val track   = Track(events)
  val sqOut   = Sequence(Vector(track))

  // sqOut.writeFile(outPath / s"test-12-1to5_seed23_Doppel_${START}_${SEED}${if (VELO) "V" else ""}E${if (INTERVALS) "I" else ""}${if (ACCEL) "A" else ""}.mid")
  sqOut.writeFile(outPath / s"${name}Doppel_${START}_${SEED}${if (VELO) "V" else ""}E${if (INTERVALS) "I" else ""}${if (ACCEL) "A" else ""}.mid")

  if (PLAY) {
    val player = Sequencer.open()
    player.play(sqOut)
    Thread.sleep(((track.duration + 1) * 1000).toLong)
    player.stop()
    player.close()
  }
}