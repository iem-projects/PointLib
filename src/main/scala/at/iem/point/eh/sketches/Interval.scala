package at.iem.point.eh.sketches

object Interval {
  implicit val ordering = Ordering.by[Interval, Int](_.semitones)
}
final class Interval(val semitones: Int) extends AnyVal {
  def modOctave: Interval = if (semitones < 12) this else new Interval(semitones % 12)

  def map(fun: Int => Int): Interval = new Interval(fun(semitones))

  // override def toString = ???
}