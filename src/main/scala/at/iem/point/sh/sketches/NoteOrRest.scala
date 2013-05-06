package at.iem.point.sh.sketches

import language.implicitConversions

object NoteOrRest {
  implicit def fromDur(dur: Int): NoteOrRest =
    if (dur < 0) Rest(-dur) else if (dur > 0) Note(dur) else sys.error("dur must not be zero")
}
sealed trait NoteOrRest {
  def dur: Int
  def toInt: Int
}
final case class Note(dur: Int) extends NoteOrRest { def toInt =  dur }
final case class Rest(dur: Int) extends NoteOrRest { def toInt = -dur }