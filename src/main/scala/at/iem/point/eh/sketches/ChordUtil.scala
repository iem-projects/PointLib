package at.iem.point.eh.sketches

import annotation.tailrec
import de.sciss.fingertree.{RangedSeq, FingerTree}

object ChordUtil {
  /** Tries to find the chords by clustering a given sequence of notes. */
  def findChords(notes: IIdxSeq[OffsetNote], minPoly: Int = 2, offsetTolerance: Double = 0.1,
                 stopTolerance: Double = 10.0): IIdxSeq[Chord] = {
    val b = IIdxSeq.newBuilder[Chord]
    @tailrec def loop(ns: IIdxSeq[OffsetNote]) {
      if (ns.isEmpty) return
      val h = ns.head
      // takes as many notes that satisfy the offset tolerance. because notes are sorted by
      // offset, this can strictly stop at the first note which doesn't satisfy the criterion.
      // note that we operate on `ns`, not `ns.tail`, therefore the result automatically
      // includes the head note itself.
      val (fit1, rest1) = ns.span(n => math.abs(n.offset - h.offset) <= offsetTolerance && n.offset < h.stop)
      // then from those notes that fit wrt offset, filter those which satisfy the
      // stop tolerance.
      val (fit, rest2)  = fit1.span(n => math.abs(n.stop - h.stop ) <= stopTolerance  )
      // if the minimum polyphony is found, add a new chord, and glue the two rest bins together
      val next = if (fit.size >= minPoly) {
        val sorted = fit.sortBy(_.pitch)
        b += Chord(sorted)
        rest2 ++ rest1
      } else ns.tail  // else drop h
      loop(next)
    }
    loop(notes)
    b.result()
  }

  /** Tries to find harmonic constellations which are vertical structures of a minimum
    * number of voices and duration
    */
  def findHarmonicFields(notes: IIdxSeq[OffsetNote], minPoly: Int = 2, minDuration: Double = 0.1): IIdxSeq[OffsetNote] = {
    if (notes.isEmpty) return Vector.empty

    /*
    Algorithm:
    - place all notes in an interval tree according to their offsets and durations
    - traverse tree and eliminate intervals which are too short

     */

//    implicit def noteInterval(n: OffsetNote) = (n.offset, n.stop)
//    val tAll = RangedSeq[OffsetNote, Double](notes.filter(_.duration >= minDuration) : _*)
//
//    implicit final class RangedOps[Elem](tree: RangedSeq[Elem, Double]) extends Any {
//      def nextAfter(point: Double): Option[Elem] = {
//        val it = tree.filterIncludes(point -> Double.PositiveInfinity)
//        if (it.hasNext) Some(it.next()) else None
//      }
//    }

    val tAll = notes.filter(_.duration >= minDuration)
    var pos = 0.0
    var filter = Vector.empty[OffsetNote]
    tAll.iterator.foreach { n =>
      val start   = n.offset
      val minStop = start + minDuration
      filter      = filter.collect {
        // no overlap, keep as is
        case n1 if n1.stop <= start => n1

        // overlaps, but begins early enough
        case n1 if start - n1.offset >= minDuration =>
          if (n1.stop >= minStop) n1 else {
            n1.replaceStop(start) // truncate end to align with current note's offset
          }

        // overlaps, but ends late enough
        case n1 if n1.stop >= minStop =>
          if (start - n1.offset >= minDuration) n1 else {
            n1.replaceStart(start) // truncate beginning to align with current note's offset
          }
      }
      filter :+= n
    }

    filter
  }
}