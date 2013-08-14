package at.iem.point.eh.sketches

import at.iem.point.illism._
import de.sciss.midi.{Sequencer, Sequence, Track, TickRate}
import de.sciss.file._

object Evolutions3 extends App {
  val NUM         = 200
  val SEED        = 2L      // seed of the random number generator
  val START       = 2       // start index in the pitch sequence to begin wih
  val VELO        = true    // model velocity
  val VELO_COARSE = 4       // velocity rasterisation (in steps)
  val ENTRY       = true    // model entry offsets
  val ENTRY_COARSE= 0.2     // entry offset rasterisation (relative, in percent 0...1)
  val ENTRY_SCALE = 1.0 // 1.5     // slow down factor if using entry modelling

  val INNER       = true    // model inner vertical structure of chords (`true`) or just frame intervals (`false`)
  val GRAINS      = true    // model inner horizontal structure of chords (`true`) or just produce monolithic blocks (`false`)

  val snippet   = staticChords(6)(1)  // improvSnippets.last  // staticChords(5).head
  val sq        = loadSnippet(snippet) // .head
  val notesIn0  = sq.notes
  val (_, h)    = NoteUtil.splitMelodicHarmonic(notesIn0)
  val hh        = h.map(_._2)

  implicit val rnd = new util.Random(SEED)
  val gen       = VerticalDance(startIdx = START, modelVelo = VELO, veloCoarse = VELO_COARSE, modelEntry = ENTRY,
    entryCoarse = ENTRY_COARSE, entryScale = ENTRY_SCALE, modelInner = INNER, modelGrains = GRAINS, chordSnippets = hh)
  val chordsOut = gen.move(NUM)

  implicit val rate = TickRate.tempo(120, 1024)
  val events  = chordsOut.flatMap(_.toMIDI)
  val track   = Track(events)
  val sqOut   = Sequence(Vector(track))

  sqOut.writeFile(outPath /
    (s"Snippet${snippet}_ChordSeq_${START}_${SEED}${if (INNER) "I" else ""}" +
     s"${if (GRAINS) "G" else ""}${if (VELO) "V" else ""}${if (ENTRY) "E" else ""}.mid"))

  val player  = Sequencer.open()
  player.play(sqOut)
  Thread.sleep(((track.duration + 1) * 1000).toLong)
  player.stop()
  player.close()
}