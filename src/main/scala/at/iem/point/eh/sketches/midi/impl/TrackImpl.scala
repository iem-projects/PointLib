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
    }
    new Impl(sq, evts.collect { case Some(m) => m}, t.ticks())
  }

  private final class Impl(val sequence: Sequence, val events: IIdxSeq[Event], val ticks: Long) extends Track {
    override def toString = s"midi.Track(# events = ${events.size}, ticks = ${ticks})@${hashCode().toHexString}"
  }
}