package at.iem.point.eh.sketches

import scala.swing.Swing
import at.iem.point.illism._

object ElisabethPartitioning extends App with ShowPartitioning {
  Swing.onEDT(run())

  def run() {
    val sn          = loadSnippet(improvSnippets(1))
    val notes       = sn.notes
    val (m, h)      = NoteUtil.splitMelodicHarmonic(notes)
    val nm          = m.flatMap(_._2)
    val nh          = h.flatMap(_._2)

    implicit val r  = sn.rate
    show(Vector(nm), Vector(nh))
  }
}
