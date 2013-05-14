package at.iem.point.ms.sketches

import de.sciss.midi
import midi.{Track, TickRate}

object NoteUtil {
//  def findMonophones(in: IIdxSeq[OffsetNote], overlapTolerance: Double = 0.1): IIdxSeq[IIdxSeq[OffsetNote]] = {
//    ???
//  }

  def toTrack(notes: IIdxSeq[OffsetNote], channel: Int = 0)(implicit tickRate: TickRate): Track = {
    val events = notes.flatMap(_.toMIDI(channel))
    Track(events)
  }

  /** Cleans a list of notes by throwing away those under a given minimum duration,
    * and adjusting the others to match note beginnings and endings within this
    * minimum duration window.
    */
  def clean(notes: IIdxSeq[OffsetNote], minDuration: Double = 0.1): IIdxSeq[OffsetNote] = {
    if (notes.isEmpty) return Vector.empty

    val tAll = notes.filter(_.duration >= minDuration)
    var tFlt = Vector.empty[OffsetNote]
    tAll.iterator.foreach { n =>
      val start   = n.offset
      val minStop = start + minDuration
      tFlt      = tFlt.collect {
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
      tFlt :+= n
    }
    tFlt
  }

  def stabbings(notes: IIdxSeq[OffsetNote]): IIdxSeq[Double] = {
    notes.flatMap(n => n.offset :: n.stop :: Nil).toSet.toIndexedSeq.sorted
  }
}