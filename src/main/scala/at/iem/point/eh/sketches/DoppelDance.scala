package at.iem.point.eh.sketches

import at.iem.point.illism._
import de.sciss.midi.TickRate

object DoppelDance {
  /** Creates a sequence generator using a context dance based on a combined vector of pitch and speed
    *
    * @param startIdx       corpus index of the element to start with
    * @param modelVelo      if `true`, models note velocity
    * @param veloCoarse     when modelling note velocity, the quantization of MIDI velocity values (1 = maximally fine)
    * @param entryCoarse    when modelling entry delays, the quantization of ?
    * @param entryScale     tempo scaling factor (< 1 faster, > 1 slower)
    * @param intervals      if `true`, uses intervals for pitch modelling, if `false` uses absolute pitches
    * @param accel          if `true`, uses acceleration for entry delay modelling, if `false` uses velocity
    * @param fuzzy          if `true`, uses a fuzzy context tree that increase vector tolerance towards the past
    * @param noteSnippets   the corpus to use
    * @param rnd            the random number generator to use
    * @return the sequence producer
    */
  def apply(startIdx: Int = 0, modelVelo: Boolean = true, veloCoarse: Int = 4,
                             entryCoarse: Double = 0.2, entryScale: Double = 1.0, intervals: Boolean = false,
                             accel: Boolean = false, fuzzy: Boolean = false,
                             noteSnippets: Vec[Vec[OffsetNote]])(implicit rnd: util.Random): DoppelDance =
    new Impl(startIdx = startIdx, modelVelo = modelVelo, veloCoarse = veloCoarse,
             entryCoarse = entryCoarse, entryScale = entryScale, intervals = intervals, accel = accel, fuzzy = fuzzy,
             m = noteSnippets)

  private final class Impl(startIdx: Int, modelVelo: Boolean, veloCoarse: Int,
                           entryCoarse: Double, entryScale: Double, intervals: Boolean, accel: Boolean,
                           fuzzy: Boolean, m: Vec[Vec[OffsetNote]])(implicit rnd: util.Random)
    extends DoppelDance {

    private val notesIn   = m.flatten // m.flatMap(_._2)

    private val pitchSq0  = notesIn.map(_.pitch.midi)
    val pitchSq = if (intervals) {
      pitchSq0.pairDiff
    } else pitchSq0

    private lazy val veloSq     = notesIn.map { n => val v = n.velocity; v - (v % veloCoarse) }
    private lazy val recVeloF   = ContextDance(veloSq)(veloSq(startIdx) :: Nil)

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

    private def expand(in: Vec[(Int, Int)]): Iterator[Traversable[(Int, Int)]] = ???

    private def expandLoop(in: Vec[(Int, Int)], bias: (Int, Int)): Iterator[Vec[(Int, Int)]] = in match {
      case init :+ ((lastPch, lastEntry)) =>
        val (biasPch, biasEntry) = bias
        val fuzzyPch    = (biasPch - 1) to (biasPch + 1)
        val coarse      = (entryCoarse * 1000).toInt
        val fuzzyEntry  = (biasEntry - coarse) to (biasEntry + coarse) by coarse
        val fuzzy       = fuzzyPch zip fuzzyEntry

        fuzzy.iterator.flatMap { b =>
          val newLast = (lastPch + biasPch, lastEntry + biasEntry)
          expandLoop(init, b).map(_ :+ newLast)
        }
      case _ => Iterator(Vec.empty)
    }

//    private val recEntryF = ContextDance(entrySq :+ entrySq.head)(entrySq(startIdx) :: Nil)
    private val zipped = pitchSq zip entrySq
    private val recPchEntryF = if (fuzzy) {
      FuzzyDance  (zipped :+ zipped.head)(zipped(startIdx) :: Nil)(expand)
    } else {
      ContextDance(zipped :+ zipped.head)(zipped(startIdx) :: Nil)
    }

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