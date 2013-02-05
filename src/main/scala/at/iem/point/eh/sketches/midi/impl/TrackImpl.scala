package at.iem.point.eh.sketches.midi
package impl

import javax.sound.{midi => j}
import collection.immutable.{IndexedSeq => IIdxSeq}
import collection.mutable

private[midi] object TrackImpl {
  def fromJava(t: j.Track, sq: Sequence): Track = {
    val sz    = t.size()
    val evts  = IIdxSeq.tabulate(sz) { i =>
      val evj = t.get(i)
      Event(evj.getTick, Message.fromJava(evj.getMessage))
    }
    new Impl(sq, evts, t.ticks())
  }

  private final class Impl(sq: Sequence, val events: IIdxSeq[Event], val ticks: Long) extends Track {
    override def toString = s"midi.Track(# events = ${events.size}, ticks = ${ticks})@${hashCode().toHexString}"

    lazy val notes: IIdxSeq[OffsetNote] = {
      val r     = sq.tickRate
      val b     = IIdxSeq.newBuilder[OffsetNote]
      val wait  = mutable.Map.empty[(Int, Int), (Double, NoteOn)]
      events.foreach {
        case Event(tick, on @ NoteOn(ch, pitch, _)) =>
          val startTime = tick / r.ticksPerSecond
          wait += (ch, pitch) -> (startTime, on)

        case Event(tick, off @ NoteOff(ch, pitch, _)) =>
          val stopTime  = tick / r.ticksPerSecond
          wait.remove(ch -> pitch).foreach { case (startTime, on) =>
            b += OffsetNote(offset = startTime, channel = ch, pitch = pitch, duration = stopTime - startTime,
              attack = on.velocity, release = off.velocity)
          }

        case _ =>
      }
      if (wait.nonEmpty) {
        println(s"Warning: pending notes ${wait.mkString("(", ",", ")")}")
      }
      b.result()
    }
  }
}