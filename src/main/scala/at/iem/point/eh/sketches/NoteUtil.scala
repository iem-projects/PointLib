package at.iem.point.eh.sketches

import midi.{Track, TickRate}

object NoteUtil {
//  def findMonophones(in: IIdxSeq[OffsetNote], overlapTolerance: Double = 0.1): IIdxSeq[IIdxSeq[OffsetNote]] = {
//    ???
//  }

  def toTrack(notes: IIdxSeq[OffsetNote], channel: Int = 0)(implicit tickRate: TickRate): Track = {
    val events = notes.flatMap(_.toMIDI(channel))
    Track(events, tickRate)
  }
}