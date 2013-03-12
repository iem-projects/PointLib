package at.iem.point.eh.sketches.midi
package impl

import javax.sound.{midi => j}
import collection.immutable.{IndexedSeq => IIdxSeq}

private[midi] object TrackImpl {
  def fromJava(t: j.Track, sq: Sequence): Track = {
    val sz    = t.size()
    val evts  = IIdxSeq.tabulate(sz) { i =>
      val evj = t.get(i)
      val j = Message.fromJavaOption(evj.getMessage)
      j.map(m => Event(evj.getTick, m))
//      val m = Message.fromJava(evj.getMessage)
//      Event(evj.getTick, m)
    }
    val ticks     = t.ticks()
    val tickRate  = sq.tickRate.copy(ticks = ticks)
    new Impl(evts.collect { case Some(m) => m}, tickRate)
//    new Impl(evts, tickRate)
  }

  def apply(events: IIdxSeq[Event], tickRate: TickRate): Track = new Impl(events, tickRate)

  private final class Impl(val events: IIdxSeq[Event], val tickRate: TickRate) extends Track {
    override def toString = s"midi.Track(# events = ${events.size}, ticks = ${tickRate.ticks})@${hashCode().toHexString}"
  }
}