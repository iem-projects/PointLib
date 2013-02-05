package at.iem.point.eh.sketches.midi

import impl.{SequencerImpl => Impl}

object Sequencer {
  def open() = Impl.open()
}
trait Sequencer {
  def play(sequence: Sequence): Unit
  def stop() : Unit
}