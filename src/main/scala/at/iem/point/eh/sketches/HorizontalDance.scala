package at.iem.point.eh.sketches

import at.iem.point.illism._
import de.sciss.midi.TickRate

object HorizontalDance {
  def apply(startIdx: Int = 0, modelVelo: Boolean = true, veloCoarse: Int = 4, modelEntry: Boolean = true,
                             entryCoarse: Double = 0.2, entryScale: Double = 1.0,
                             noteSnippets: Vec[Vec[OffsetNote]])(implicit rnd: util.Random): HorizontalDance =
    new Impl(startIdx = startIdx, modelVelo = modelVelo, veloCoarse = veloCoarse, modelEntry = modelEntry,
             entryCoarse = entryCoarse, entryScale = entryScale, m = noteSnippets)

  private final class Impl(startIdx: Int, modelVelo: Boolean, veloCoarse: Int, modelEntry: Boolean,
                           entryCoarse: Double, entryScale: Double,
                           m: Vec[Vec[OffsetNote]])(implicit rnd: util.Random)
    extends HorizontalDance {

    private val notesIn   = m.flatten // m.flatMap(_._2)

    private val pitchSq   = notesIn.map(_.pitch.midi)
    private val recPchF   = ContextDance(pitchSq)(pitchSq(startIdx) :: Nil)

    private val veloSq    = notesIn.map { n => val v = n.velocity; v - (v % veloCoarse) }
    private val recVeloF  = ContextDance(veloSq)(veloSq(startIdx) :: Nil)

    private val entrySq   = notesIn.sliding(2, 1).to[Vector].map { case Seq(a, b) =>
      val fine    = ((b.offset - a.offset) * 1000 + 0.5).toInt // millis
      val coarse  = (fine * entryCoarse + 0.5).toInt
      if (coarse > 0) (fine - fine % coarse) else fine
//      millis * 1000
    }
    private val recEntryF = ContextDance(entrySq :+ entrySq.head)(entrySq(startIdx) :: Nil)

    private var produced  = 0

    def move(num: Int): Vector[OffsetNote] = {
      val recPch = recPchF.move(num)

      val notesOut0 = recPch.zipWithIndex.map { case (midi, idx) =>
        val off = (idx + produced) * 0.25
        OffsetNote(off, midi.asPitch, 0.125, 80)
      }

      val notesOut1 = if (modelVelo) {
        val veloSq  = notesIn.map { n => val v = n.velocity; v - (v % veloCoarse) }
        val recVelo = recVeloF.move(num = num)
        (notesOut0 zip recVelo).map { case (n, v) => n.copy(velocity = v) }

      } else notesOut0

      implicit val rate = TickRate.tempo(120, 1024)

    //  val sorted = notesIn.sortBy(_.offset)
    //  assert(notesIn == sorted)

      val notesOut  = if (modelEntry) {
    //    println(entrySq.mkString(", "))
    //    ContextDance.DEBUG = true
        val recEntry = recEntryF.move(num = num)
    //    println(recEntry.mkString(", "))
        var off = 0.0
        (notesOut1 zip recEntry).map { case (n, e) =>
          val res = n.copy(offset = off)
          off += e * 0.001 * entryScale
          res
        }

      } else notesOut1

      produced += num
      notesOut
    }
  }
}
sealed trait HorizontalDance {
  def move(num: Int): Vector[OffsetNote]
}