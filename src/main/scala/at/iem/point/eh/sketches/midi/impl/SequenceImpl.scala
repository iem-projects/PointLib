package at.iem.point.eh.sketches.midi
package impl

import javax.sound.{midi => j}
import collection.immutable.{IndexedSeq => IIdxSeq}
import collection.breakOut

private[midi] object SequenceImpl {
  def fromJava(sj: j.Sequence): Sequence = new Impl(sj)

  private final class Impl(val peer: j.Sequence) extends Sequence {
    self =>

    override def toString = s"midi.Sequence(# tracks = ${peer.getTracks.length})@${hashCode().toHexString}"

    lazy val tracks: IIdxSeq[Track] = peer.getTracks.map(tj => TrackImpl.fromJava(tj, self))(breakOut)

    def tickRate = TickRate(ticks = peer.getTickLength, micros = peer.getMicrosecondLength)

//    def notes: IIdxSeq[OffsetNote] = tracks.flatMap(_.notes)

    def toJava: j.Sequence = peer
  }
}