package at.iem.point.eh.sketches.midi

final case class TickRate(ticks: Long, micros: Long) {
  override def toString = s"${productPrefix}(ticks = ${ticks}, micros = ${micros})"
  def ticksPerSecond: Double = {
    val sec = micros * 1.0e-6
    ticks / sec
  }
}