package at.iem.point.eh.sketches.midi
package impl

import javax.sound.{midi => j}

private[midi] object InstrumentImpl {
  def fromJava(ij: j.Instrument): Instrument = new Impl(ij)

  private final class Impl(val peer: j.Instrument) extends Instrument {
    override def toString = s"midi.Instrument(name = ${name})"

    def name: String = peer.getName
  }
}