package at.iem.point.eh.sketches

sealed trait NoteLike {
//  /**
//   * MIDI channel (0 to 15)
//   */
//  def channel: Int

  /**
   * MIDI pitch number
   */
  def pitch: Pitch

  /**
   * Duration __in seconds__
   */
  def duration: Double

  /**
   * MIDI attack velocity
   */
  def velocity: Int

//  /**
//   * MIDI release velocity (typically zero)
//   */
//  def release: Int

  /**
   * Returns the duration rounded to milliseconds, as a String.
   */
  final def durationString: String = s"${duration.roundSecondsToMillis}s"

  final def noteOn(channel: Int):  midi.NoteOn   = midi.NoteOn (channel, pitch.midi, velocity)
  final def noteOff(channel: Int): midi.NoteOff  = midi.NoteOff(channel, pitch.midi, 0)
}

/**
 * A logical grouping of a note on and note off.
 *
 * @param pitch     the MIDI key number (pitch)
 * @param duration  the duration __in seconds__
 * @param velocity    the attack velocity
 */
final case class Note(/* channel: Int, */ pitch: Pitch, duration: Double, velocity: Int /*, release: Int = 0 */) extends NoteLike {
  override def toString = {
    s"${productPrefix}(${pitch}, dur = ${durationString}, vel = $velocity})"
  }

  def withOffset(offset: Double): OffsetNote =
    OffsetNote(offset = offset, /* channel = channel, */ pitch = pitch,
      duration = duration, velocity = velocity /*, release = release */)
}

final case class OffsetNote(offset: Double, /* channel: Int, */ pitch: Pitch, duration: Double, velocity: Int /*, release: Int = 0 */)
  extends NoteLike {

  override def toString = {
    s"${productPrefix}(${pitch}, off = ${offsetString}, dur = ${durationString}, vel = ${velocity})"
  }

  def replaceStart(newOffset: Double): OffsetNote = {
    val newDuration = duration - (newOffset - offset)
    copy(offset = newOffset, duration = newDuration)
  }

  def replaceStop(newStop: Double): OffsetNote = {
    val newDuration = newStop - offset
    copy(duration = newDuration)
  }

  def offsetString: String = s"${offset.roundSecondsToMillis}s"

  def stop: Double = offset + duration

  def dropOffset: Note = Note(/* channel = channel, */ pitch = pitch, duration = duration, velocity = velocity)
}