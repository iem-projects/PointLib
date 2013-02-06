package at.iem.point.eh.sketches

object Pitch {
  var displayLanguage: Language = Language.English

  implicit val ordering = Ordering.by[Pitch, Int](_.midi)
}
final class Pitch(val midi: Int) extends AnyVal {
  override def toString = midi.pitchString(Pitch.displayLanguage)

  def interval(that: Pitch): Interval = new Interval(math.abs(this.midi - that.midi))

  def map(fun: Int => Int): Pitch = new Pitch(fun(midi))
}