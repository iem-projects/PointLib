package at.iem.point.eh.sketches.midi
package impl

import javax.sound.{midi => j}
import collection.breakOut

private[midi] object SynthesizerImpl {
  def open(): Synthesizer = {
    val sj = j.MidiSystem.getSynthesizer
    if (!sj.isOpen) sj.open()
    fromJava(sj)
  }

  def fromJava(synth: j.Synthesizer): Synthesizer = {
    new Impl(synth)
  }

  private final class Impl(val peer: j.Synthesizer) extends Synthesizer {
    override def toString = s"midi.Synthesizer(${peer.getDeviceInfo.getName})@${hashCode().toHexString}"

    def close() { peer.close() }

    lazy val instruments: Map[String, Instrument] = {
      peer.getAvailableInstruments.map(ij => ij.getName -> InstrumentImpl.fromJava(ij))(breakOut)
    }

    def load(instrument: Instrument) {
      peer.loadInstrument(instrument.peer)
    }
  }
}