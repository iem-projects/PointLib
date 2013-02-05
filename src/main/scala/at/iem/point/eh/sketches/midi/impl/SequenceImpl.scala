package at.iem.point.eh.sketches.midi
package impl

import javax.sound.{midi => j}
import collection.immutable.{IndexedSeq => IIdxSeq}
import collection.breakOut

private[midi] object SequenceImpl {
  def fromJava(seq: j.Sequence): Sequence = {
    val tsj = seq.getTracks
    val ts: IIdxSeq[Track] = tsj.map(TrackImpl.fromJava)(breakOut)
    new Impl(ts)
  }

  private final class Impl(val tracks: IIdxSeq[Track]) extends Sequence {
    override def toString = s"midi.Sequence(# tracks = ${tracks.size})@${hashCode().toHexString}"
  }
}