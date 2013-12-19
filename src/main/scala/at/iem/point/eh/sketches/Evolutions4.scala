package at.iem.point.eh.sketches

import at.iem.point.illism._
import de.sciss.midi.{Sequencer, Sequence, Track, TickRate}
import de.sciss.file._
import de.sciss.kollflitz.RandomOps._

object Evolutions4 extends App {
  val NUM         = 200
  val SEED        = 13L      // seed of the random number generator
  val START       = 2       // start index in the pitch sequence to begin wih
  val VELO        = true    // model velocity
  val VELO_COARSE = 4       // velocity rasterisation (in steps)
  val ENTRY       = true    // model entry offsets
  val ENTRY_COARSE= 0.2     // entry offset rasterisation (relative, in percent 0...1)
  val ENTRY_SCALE = 1.25    // 1.5     // slow down factor if using entry modelling

  val INNER       = true    // model inner vertical structure of chords (`true`) or just frame intervals (`false`)
  val GRAINS      = true    // model inner horizontal structure of chords (`true`) or just produce monolithic blocks (`false`)

  //  val snippets  = improvSnippets.last :: staticChords(5).last :: Nil  // staticChords(5).head
  val snippets  = /* staticChords(5).last :: */ improvSnippets  // staticChords(5).head
  var notesIn0  = Vector.empty[OffsetNote]
  snippets.foreach { i =>
    val sn  = loadSnippet(i).notes
    val off = notesIn0.lastOption.map(n => n.stop + n.duration).getOrElse(0.0)
    val n2  = sn.map(n => n.copy(offset = n.offset + off))
    notesIn0 ++= n2
  }
  val (m, h)    = NoteUtil.splitMelodicHarmonic(notesIn0)
  val mm        = m.map(_._2)
  val hh        = h.map(_._2)

  val mNum      = m.map(_._2.size)
  val hNum      = h.map(_._2.size)

  implicit val rnd = new util.Random(SEED)

  val genM      = HorizontalDance(startIdx = START, modelVelo = VELO, veloCoarse = VELO_COARSE, modelEntry = ENTRY,
  entryCoarse = ENTRY_COARSE, entryScale = ENTRY_SCALE, noteSnippets = mm)
  val genH      = VerticalDance(startIdx = START, modelVelo = VELO, veloCoarse = VELO_COARSE, modelEntry = ENTRY,
    entryCoarse = ENTRY_COARSE, entryScale = ENTRY_SCALE, modelInner = INNER, modelGrains = GRAINS, chordSnippets = hh)

  var produced  = 0
  var tpe       = 'h' // alternates between 'h' and 'm'
  var res       = Vector.empty[OffsetNote]
  while (produced < NUM) {
    val (notes, num) = if (tpe == 'h') {
      val _num = mNum.choose()
      genM.move(_num) -> _num
    } else {
      val _num = hNum.choose()
      genH.move(_num).flatMap(_.notes).sortBy(_.offset) -> _num
    }
    val off  = res.lastOption.map(n => n.stop + n.duration).getOrElse(0.0)
    val d    = off - notes.head.offset
    res    ++= notes.map(n => n.copy(offset = n.offset + d))
    produced += num
    tpe = if (tpe == 'h') 'm' else 'h'
  }

  implicit val rate = TickRate.tempo(120, 1024)
  val events  = res.flatMap(_.toMIDI)
  val track   = Track(events)
  val sqOut   = Sequence(Vector(track))

  //  sqOut.writeFile(outPath /
  //    (s"Snippet${snippet}_ChordSeq_${START}_${SEED}${if (INNER) "I" else ""}" +
  //     s"${if (GRAINS) "G" else ""}${if (VELO) "V" else ""}${if (ENTRY) "E" else ""}.mid"))

  sqOut.writeFile(outPath /
    (s"Snippet${snippets.mkString("_")}_Resynth_${START}_${SEED}${if (INNER) "I" else ""}" +
     s"${if (GRAINS) "G" else ""}${if (VELO) "V" else ""}${if (ENTRY) "E" else ""}.mid"))

  val player  = Sequencer.open()
  player.play(sqOut)
  Thread.sleep(((track.duration + 1) * 1000).toLong)
  player.stop()
  player.close()
}