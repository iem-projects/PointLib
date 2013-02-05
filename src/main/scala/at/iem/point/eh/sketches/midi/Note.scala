package at.iem.point.eh.sketches.midi

final case class Note(channel: Int, key: Int, duration: Int, attack: Int, release: Int = 0) {
  override def toString = {
    val velo = if (release == 0) s"velocity = ${attack}" else s"$attack = ${attack}, release = ${release}"
    s"${productPrefix}(channel = ${channel}, key = ${key} / ${key.pitchString()}, duration = ${duration}, ${velo})"
  }

  def noteOn:  NoteOn   = NoteOn (channel, key, attack )
  def noteOff: NoteOff  = NoteOff(channel, key, release)
}
