package at.iem.point.eh.sketches.midi
package impl

import javax.sound.{midi => j}

private[midi] object SequencerImpl {
  def open(): Sequencer = {
    val sj = j.MidiSystem.getSequencer
    if (!sj.isOpen) sj.open()
    fromJava(sj)
  }

  def fromJava(sj: j.Sequencer): Sequencer = new Impl(sj)

  private final class Impl(val peer: j.Sequencer) extends Sequencer {
    override def toString = s"midi.Sequencer(${peer.getDeviceInfo.getName})@${hashCode().toHexString}"

    def isPlaying: Boolean = peer.isRunning

    def play(sequence: Sequence) {
      if (isPlaying) stop()
      peer.setSequence(sequence.toJava)
      peer.start()
    }

    def stop() {
      peer.stop()
    }

    def close() { peer.close() }
  }
}