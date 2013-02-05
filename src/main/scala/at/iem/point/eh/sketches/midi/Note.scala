package at.iem.point.eh.sketches.midi

sealed trait NoteLike {
  /**
   * MIDI channel (0 to 15)
   */
  def channel: Int

  /**
   * MIDI pitch number
   */
  def pitch: Int

  /**
   * Duration __in seconds__
   */
  def duration: Double

  /**
   * MIDI attack velocity
   */
  def attack: Int

  /**
   * MIDI release velocity (typically zero)
   */
  def release: Int

  /**
   * Returns the duration rounded to milliseconds, as a String.
   */
  final def durationString: String = s"${duration.roundSecondsToMillis}s"

  final def noteOn:  NoteOn   = NoteOn (channel, pitch, attack )
  final def noteOff: NoteOff  = NoteOff(channel, pitch, release)
}

/**
 * A logical grouping of a note on and note off.
 *
 * @param channel   the MIDI channel (0 to 15)
 * @param pitch     the MIDI key number (pitch)
 * @param duration  the duration __in seconds__
 * @param attack    the attack velocity
 * @param release   the release velocity (defaults to `0`)
 */
final case class Note(channel: Int, pitch: Int, duration: Double, attack: Int, release: Int = 0) extends NoteLike {
  override def toString = {
    val velo = if (release == 0) s"velocity = ${attack}" else s"$attack = ${attack}, release = ${release}"
    s"${productPrefix}(channel = ${channel}, pitch = ${pitch} : ${pitch.pitchString()}, duration = ${durationString}, ${velo})"
  }

  def withOffset(offset: Double): OffsetNote =
    OffsetNote(offset = offset, channel = channel, pitch = pitch,
      duration = duration, attack = attack, release = release)
}

final case class OffsetNote(offset: Double, channel: Int, pitch: Int, duration: Double, attack: Int, release: Int = 0)
  extends NoteLike {

  override def toString = {
    val velo = if (release == 0) s"velocity = ${attack}" else s"$attack = ${attack}, release = ${release}"
    s"${productPrefix}(offset = ${offsetString}, channel = ${channel}, pitch = ${pitch} : ${pitch.pitchString()}, duration = ${durationString}, ${velo})"
  }

  def offsetString: String = s"${offset.roundSecondsToMillis}s"

  def stop: Double = offset + duration

  def dropOffset: Note = Note(channel = channel, pitch = pitch, duration = duration, attack = attack, release = release)
}