package at.iem.point.eh.sketches

import at.iem.point.illism._
import de.sciss.midi.TickRate

object BreakDance {
  def apply(startIdx: Int = 0, modelVelo: Boolean = true, veloCoarse: Int = 6, modelEntry: Boolean = true,
            entryCoarse: Double = 2, entryScale: Double = 1.0,
            notesIn: Vec[OffsetNote])(implicit rnd: util.Random): HorizontalDance =
    new Impl(startIdx = startIdx, modelVelo = modelVelo, veloCoarse = veloCoarse, modelEntry = modelEntry,
      entryCoarse = entryCoarse, entryScale = entryScale, notesIn = notesIn)

  private final class Impl(startIdx: Int, modelVelo: Boolean, veloCoarse: Int, modelEntry: Boolean,
                           entryCoarse: Double, entryScale: Double,
                           notesIn: Vec[OffsetNote])(implicit rnd: util.Random)
    extends HorizontalDance {

    // private val notesIn   = m.flatten // m.flatMap(_._2)

    private val pitchSq   = notesIn.map(_.pitch.midi)
    private val recPchF   = ContextDance(pitchSq)(pitchSq(startIdx) :: Nil)

    private val veloSq    = notesIn.map { n => val v = n.velocity; v - (v % veloCoarse) }
    private val recVeloF  = ContextDance(veloSq)(veloSq(startIdx) :: Nil)

    private def quantDur(in: Double): Int = (math.log(math.max(1, in/0.010)) / math.log(2) * entryCoarse + 0.5).toInt
    private def unQuantDur(q: Int): Double = math.exp(math.log(2) * q / entryCoarse) * 0.010

    private val entrySq   = notesIn.sliding(2, 1).to[Vector].map { case Seq(a, b) =>
      val en  = b.offset - a.offset
      val du  = a.duration
      val enQ = quantDur(en)
      val duQ = quantDur(du)
      (enQ, duQ)
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
        // val veloSq  = notesIn.map { n => val v = n.velocity; v - (v % veloCoarse) }
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
        (notesOut1 zip recEntry).map { case (n, (enQ, duQ)) =>
          val en  = unQuantDur(enQ)
          val du  = unQuantDur(duQ)
          val res = n.copy(offset = off, duration = du * entryScale)
          off += en * entryScale
          res
        }

      } else notesOut1

      produced += num
      notesOut
    }
  }
}