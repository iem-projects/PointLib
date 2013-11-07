package at.iem.point.ms.sketches

import at.iem.point.illism._
import scala.annotation.tailrec

object NoteUtil2 {
  implicit val noteOffsetView = (n: OffsetNote) => n.offset

  /** Produces note groups by applying a sliding time window with given
    * window length (`size`) and step size (`step`) in seconds.
    */
  def slidingWindow[A](notes: Vec[A], size: Double, step: Double)
                      (implicit view: A => Double): Vec[Vec[A]] = {
    if (notes.isEmpty) return Vector.empty

    type Notes      = Vec[A]
    type NoteGroups = Vec[Notes]

    require(step > 0 && step <= size)

    def take(xs: Notes, stop : Double): Notes = xs.takeWhile(view(_) < stop )
    def drop(xs: Notes, start: Double): Notes = xs.dropWhile(view(_) < start)

    @tailrec def loop(xs: Notes, offset: Double, res: NoteGroups): NoteGroups = {
      if (xs.isEmpty) res else {
        val group = take(xs, stop  = offset + size)
        val tail  = drop(xs, start = offset + step)
        loop(tail, offset + step, res :+ group)
      }
    }

    loop(notes, offset = view(notes.head), res = Vector.empty)
  }
}