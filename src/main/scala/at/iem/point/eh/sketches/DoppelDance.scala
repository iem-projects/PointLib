package at.iem.point.eh.sketches

import at.iem.point.illism._
import de.sciss.midi.TickRate

object DoppelDance {
  def apply(startIdx: Int = 0, modelVelo: Boolean = true, veloCoarse: Int = 4, modelEntry: Boolean = true,
                             entryCoarse: Double = 0.2, entryScale: Double = 1.0, intervals: Boolean = false,
                             accel: Boolean = false,
                             noteSnippets: Vec[Vec[OffsetNote]])(implicit rnd: util.Random): DoppelDance =
    new Impl(startIdx = startIdx, modelVelo = modelVelo, veloCoarse = veloCoarse, modelEntry = modelEntry,
             entryCoarse = entryCoarse, entryScale = entryScale, intervals = intervals, accel = accel,
             m = noteSnippets)

  private final class Impl(startIdx: Int, modelVelo: Boolean, veloCoarse: Int, modelEntry: Boolean,
                           entryCoarse: Double, entryScale: Double, intervals: Boolean, accel: Boolean,
                           m: Vec[Vec[OffsetNote]])(implicit rnd: util.Random)
    extends DoppelDance {

    private val notesIn   = m.flatten // m.flatMap(_._2)

    private val pitchSq0  = notesIn.map(_.pitch.midi)
    val pitchSq = if (intervals) {
      pitchSq0.pairDiff
    } else pitchSq0

    private val veloSq    = notesIn.map { n => val v = n.velocity; v - (v % veloCoarse) }
    private val recVeloF  = ContextDance(veloSq)(veloSq(startIdx) :: Nil)

    private val entrySq   = if (accel) {
      val firstDur = notesIn(1).offset - notesIn(0).offset
      ((Vec.empty[Int], firstDur) /: notesIn.sliding(2, 1).to[Vector]) { case ((res, prevDur), Seq(a, b)) =>
          val dur     = b.offset - a.offset
          val accel   = dur / firstDur
          val fine    = (accel * 1000 * entryCoarse).toInt
          // val coarse  = (fine * entryCoarse + 0.5).toInt
          val v       = fine // if (coarse > 0) fine - fine % coarse else fine
          println(s"dur = $dur, accel = $accel, fine = $fine")
         (res :+ v, dur)
      } ._1
    } else {
      notesIn.sliding(2, 1).to[Vector].map { case Seq(a, b) =>
        val fine    = ((b.offset - a.offset) * 1000 + 0.5).toInt // millis
        val coarse  = (fine * entryCoarse + 0.5).toInt
        if (coarse > 0) fine - fine % coarse else fine
        //      millis * 1000
      }
    }

//    private val recEntryF = ContextDance(entrySq :+ entrySq.head)(entrySq(startIdx) :: Nil)
    val zipped = pitchSq zip entrySq
    private val recPchEntryF = ContextDance(zipped :+ zipped.head)(zipped(startIdx) :: Nil)

    private var produced  = 0

    def move(num: Int): Vector[OffsetNote] = {
      val recPchEntry = recPchEntryF.move(num)

      var off = 0.0
      var midiPred = 80 // XXX TODO: make configurable
      var prevDur = notesIn(1).offset - notesIn(0).offset
      val notesOut0 = recPchEntry.zipWithIndex.map { case ((midi0, e), idx) =>
//        val off = (idx + produced) * 0.25
//          val res = n.copy(offset = off)
        val midi = if (intervals) {
          val res = midiPred
          midiPred += midi0
          math.max(0, math.min(127, res))
        } else midi0

        val res = OffsetNote(off, midi.asPitch, 0.125, 80)
        if (accel) {
          val e1 = e * (0.001 / entryCoarse)
          prevDur = math.min(1.41, math.max(0.01, e1 * prevDur))
          off += prevDur * entryScale
        } else {
          off += e * 0.001 * entryScale
        }
        res
      }

      val notesOut1 = if (modelVelo) {
        val veloSq  = notesIn.map { n => val v = n.velocity; v - (v % veloCoarse) }
        val recVelo = recVeloF.move(num = num)
        (notesOut0 zip recVelo).map { case (n, v) => n.copy(velocity = v) }

      } else notesOut0

      implicit val rate = TickRate.tempo(120, 1024)

    //  val sorted = notesIn.sortBy(_.offset)
    //  assert(notesIn == sorted)

      val notesOut  =
//        if (modelEntry) {
//    //    println(entrySq.mkString(", "))
//    //    ContextDance.DEBUG = true
//        val recEntry = recEntryF.move(num = num)
//    //    println(recEntry.mkString(", "))
//        var off = 0.0
//        (notesOut1 zip recEntry).map { case (n, e) =>
//          val res = n.copy(offset = off)
//          off += e * 0.001 * entryScale
//          res
//        }
//
//      } else {
        notesOut1

      produced += num
      notesOut
    }
  }
}
sealed trait DoppelDance {
  def move(num: Int): Vector[OffsetNote]
}