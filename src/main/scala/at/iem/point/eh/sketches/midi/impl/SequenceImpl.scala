package at.iem.point.eh.sketches.midi
package impl

import javax.sound.{midi => j}
import collection.immutable.{IndexedSeq => IIdxSeq}
import collection.breakOut

private[midi] object SequenceImpl {
  def fromJava(sj: j.Sequence): Sequence = new FromJava(sj)

  def apply(tracks: IIdxSeq[Track]): Sequence = {
    tracks match {
      case head +: tail =>
        require(tail.forall(_.tickRate.micros == head.tickRate), "Cannot mix tracks with different time bases")
        val ticks     = (0L /: tail) { case (m, t) => math.max(m, t.tickRate.ticks) }
        val tickRate  = head.tickRate.copy(ticks = ticks)
        new Apply(tracks, tickRate)

      case _ =>
        sys.error("Sequences with no tracks currently not supported")
    }
  }

  private sealed trait Impl extends Sequence {
    protected def numTracks: Int

    override def toString = s"midi.Sequence(# tracks = $numTracks)@${hashCode().toHexString}"
  }

  private final class Apply(val tracks: IIdxSeq[Track], val tickRate: TickRate) extends Impl {
    protected def numTracks = tracks.size

    def toJava = ???
  }

  private final class FromJava(val peer: j.Sequence) extends Impl {
    self =>

    protected def numTracks = peer.getTracks.length

    lazy val tracks: IIdxSeq[Track] = peer.getTracks.map(tj => TrackImpl.fromJava(tj, self))(breakOut)

    def tickRate = TickRate(ticks = peer.getTickLength, micros = peer.getMicrosecondLength)

//    def notes: IIdxSeq[OffsetNote] = tracks.flatMap(_.notes)

    def toJava: j.Sequence = peer
  }
}