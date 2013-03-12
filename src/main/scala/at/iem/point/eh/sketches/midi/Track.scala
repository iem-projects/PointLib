package at.iem.point.eh.sketches.midi

import impl.{TrackImpl => Impl}
import collection.immutable.{IndexedSeq => IIdxSeq}

object Track {
  def apply(events: IIdxSeq[Event], tickRate: TickRate): Track = Impl(events, tickRate)
}
trait Track {
  def events: IIdxSeq[Event]
  def tickRate: TickRate
}