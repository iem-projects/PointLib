package at.iem.point.eh.sketches.midi

import impl.{SynthesizerImpl => Impl}
import javax.sound.{midi => j}

object Synthesizer {
  def open(): Synthesizer = Impl.open()
  def piano(): Synthesizer = {
    val res = open()
    res.load(res.instruments("Piano"))
    res
  }
}
trait Synthesizer {
  private[midi] def peer: j.Synthesizer
  def close(): Unit
  def instruments: Map[String, Instrument]
  def load(instrument: Instrument): Unit
}