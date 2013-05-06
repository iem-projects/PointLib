package at.iem.point.sh.sketches

import language.implicitConversions
import spire.math.Rational

object NoteOrRest {
  implicit def fromInt(i: Int): NoteOrRest =
    if (i < 0) Rest(-i) else if (i > 0) Note(i) else sys.error("dur must not be zero")
}
sealed trait NoteOrRest {
  def dur: Rational
  def toNumber: Rational
  def *(factor: Rational): NoteOrRest
}
final case class Note(dur: Rational) extends NoteOrRest {
  def toNumber =  dur
  def *(factor: Rational): Note = copy(dur * factor)
}
final case class Rest(dur: Rational) extends NoteOrRest {
  def toNumber = -dur
  def *(factor: Rational): Rest = copy(dur * factor)
}