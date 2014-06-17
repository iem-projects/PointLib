package at.iem.point.eh.sketches

import at.iem.point.illism._
import de.sciss.midi.{Sequencer, Sequence, Track, TickRate}
import de.sciss.file._

object Evolutions6 extends App {
  // with start 0, seeds of 1, 2 creates funny loops; 0 and 3 have many walks, 4 is great because it keeps looping but then escapes
  val NUM         = 500
  val SEED        = 333L    // seed of the random number generator
  val START       = 0       // start index in the pitch sequence to begin wih
  val VELO        = true    // model velocity
  val VELO_COARSE = 6       // velocity rasterisation (in steps)
  val ENTRY       = true    // model entry offsets
  val ENTRY_COARSE= 2.0     // entry offset rasterisation (divisions per time-octave)

  /*
  snippets: 9, 42, 43, 44, 45, 48
  'disklavier'
    4_TeilweisePedal
    Cluster
    Kreisend
    SchleifenderDaumen
    StummeTasten

   */

  val sn1  = ((0 to 5) diff Seq(4)).map(loadDisklavier)
  val sn2  = Vec(9, 42, 43, 44, 45, 48).map(loadSnippet)
  // val name = "DisklavierMix"
  // val ENTRY_SCALE = 1.5
  // val sn3  = sn1
  // val name = "SnippetMix"
  // val ENTRY_SCALE = 1.1
  // val sn3  = sn2
  val name = "ImprovMix"
  val ENTRY_SCALE = 1.5
  val sn3  = sn1 ++ sn2

  // val sq        = if (DISKLAVIER) loadDisklavier(IDX) else loadSnippet(improvSnippets(IDX))
  val notesIn: Vec[OffsetNote] = joinSnippets(sn3)

  implicit val rnd  = new util.Random(SEED)
  val gen       = BreakDance(startIdx = START, modelVelo = VELO, veloCoarse = VELO_COARSE, modelEntry = ENTRY,
    entryCoarse = ENTRY_COARSE, entryScale = ENTRY_SCALE, notesIn = notesIn)
  val notesOut  = gen.move(NUM)

  implicit val rate = TickRate.tempo(120, 1024)
  val events  = notesOut.flatMap(_.toMIDI)
  val track   = Track(events)
  val sqOut   = Sequence(Vector(track))

  sqOut.writeFile(outPath / s"${name}_Evo6_${START}_${SEED}${if (VELO) "V" else ""}${if (ENTRY) "E" else ""}.mid")

  val player  = Sequencer.open()
  player.play(sqOut)
  Thread.sleep(((track.duration + 1) * 1000).toLong)
  player.stop()
  player.close()
}