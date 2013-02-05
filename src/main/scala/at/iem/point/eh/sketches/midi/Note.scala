package at.iem.point.eh.sketches.midi

final case class Note(channel: Int, key: Int, duration: Int, attack: Int, release: Int) {
  override def toString = s"${productPrefix}(channel = ${channel}, key = ${key} / ${key.pitchString()}, duration = ${duration}, attack = ${attack}, release = ${release})"

  def noteOn:  NoteOn   = NoteOn (channel, key, attack )
  def noteOff: NoteOff  = NoteOff(channel, key, release)
}