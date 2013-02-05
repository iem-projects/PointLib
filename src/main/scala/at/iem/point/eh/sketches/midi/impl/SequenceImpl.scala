package at.iem.point.eh.sketches.midi
package impl

import javax.sound.{midi => j}
import collection.immutable.{IndexedSeq => IIdxSeq}
import collection.breakOut

private[midi] object SequenceImpl {
  def fromJava(sj: j.Sequence): Sequence = new Impl(sj)

  private final class Impl(val peer: j.Sequence) extends Sequence {
    override def toString = s"midi.Sequence(# tracks = ${peer.getTracks.length})@${hashCode().toHexString}"

    lazy val tracks: IIdxSeq[Track] = peer.getTracks.map(TrackImpl.fromJava)(breakOut)

    def toJava: j.Sequence = peer
  }
}