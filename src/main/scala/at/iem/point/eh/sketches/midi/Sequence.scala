package at.iem.point.eh.sketches.midi

import impl.{SequenceImpl => Impl}
import java.io.File
import javax.sound.{midi => j}
import collection.immutable.{IndexedSeq => IIdxSeq}

object Sequence {
  /**
   * Reads a sequence from an external MIDI file.
   *
   * @param path  Path of a standard MIDI file.
   *
   * @return  the file parsed as a `Sequence`
   */
  def read(path: String): Sequence = read(new File(path))

  /**
   * Reads a sequence from an external MIDI file.
   *
   * @param file  a standard MIDI file.
   *
   * @return  the file parsed as a `Sequence`
   */
  def read(file: File): Sequence = {
    Impl.fromJava(j.MidiSystem.getSequence(file))
  }
}
trait Sequence {
  /**
   * All tracks of the sequence.
   */
  def tracks: IIdxSeq[Track]
}