package at.iem.point.eh.sketches.midi

import javax.sound.{midi => j}

trait Instrument {
  def name: String
  private[midi] def peer: j.Instrument
}