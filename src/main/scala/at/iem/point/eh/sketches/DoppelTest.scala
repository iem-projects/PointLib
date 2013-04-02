package at.iem.point.eh.sketches

import at.iem.point.illism._
import de.sciss.midi.{Sequencer, Sequence, Track, TickRate}

object DoppelTest extends App {
  // with start 0, seeds of 1, 2 creates funny loops; 0 and 3 have many walks, 4 is great because it keeps looping but then escapes
  val NUM         = 200
  val SEED        = 6L      // seed of the random number generator
  val START       = 0       // start index in the pitch sequence to begin wih
  val VELO        = false // true    // model velocity
  val VELO_COARSE = 4       // velocity rasterisation (in steps)
  val ENTRY       = true // true    // model entry offsets
  val ENTRY_COARSE= 0.2     // entry offset rasterisation (relative, in percent 0...1)
  val ENTRY_SCALE = 1.25    // slow down factor if using entry modelling

  val sq        = loadSnippet(improvSnippets.last)
  val notesIn0  = sq.notes
  val (m, _)    = NoteUtil.splitMelodicHarmonic(notesIn0)
  val mm        = m.map(_._2)

  implicit val rnd  = new util.Random(SEED)
  val gen       = DoppelDance(startIdx = START, modelVelo = VELO, veloCoarse = VELO_COARSE, modelEntry = ENTRY,
  entryCoarse = ENTRY_COARSE, entryScale = ENTRY_SCALE, noteSnippets = mm)
  val notesOut  = gen.move(NUM)

  implicit val rate = TickRate.tempo(120, 1024)
  val events  = notesOut.flatMap(_.toMIDI)
  val track   = Track(events)
  val sqOut   = Sequence(Vector(track))

  sqOut.writeFile(outPath / s"Improv_PitchSeq_${START}_${SEED}${if (VELO) "V" else ""}${if (ENTRY) "E" else ""}.mid")

  val player  = Sequencer.open()
  player.play(sqOut)
  Thread.sleep(((track.duration + 1) * 1000).toLong)
  player.stop()
  player.close()
}