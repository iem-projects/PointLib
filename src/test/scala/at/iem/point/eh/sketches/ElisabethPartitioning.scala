package at.iem.point.eh.sketches

import scala.swing.Swing
import at.iem.point.illism._
import scala.annotation.tailrec

object ElisabethPartitioning extends App with ShowPartitioning {
  Swing.onEDT(run())

  def test1() {
    val sn          = loadSnippet(improvSnippets(1))
    val notes       = sn.notes
    val (m, h)      = NoteUtil.splitMelodicHarmonic(notes)
    val nm          = m.flatMap(_._2)
    val nh          = h.flatMap(_._2)

    implicit val r  = sn.rate
    show(Vector(nm), Vector(nh))
  }

  def test2() {
    // 0 = kreisend, 1 = cluster, 2 = stumme tasten, 3 = teilweise pedal, 4 = mit obersten hoehen, 5 = schleifender daumen
    val sn              = loadDisklavier(5)
    val notes           = sn.notes
    val (black, white)  = notes.partition(n =>
      n.pitch.`class`.step match {
        case 1 | 3 | 6 | 8 | 10 => true
        case _                  => false
      }
    )

    implicit val r = sn.rate
    show(Vector(white, black), Vector.empty)
  }

  def run() {
    val sn    = loadDisklavier(0)
    val notes = sn.notes

    def inHeap(minTime: Double, maxTime: Double, minPitch: Int, maxPitch: Int)(n: OffsetNote): Boolean = {
      val p = n.pitch.midi
      n.offset < maxTime && n.stop > minTime && p >= minPitch && p <= maxPitch
    }

    val timeTol   = 0.3
    val pitchTol  = 3

    def mkHeap(rem: IIdxSeq[OffsetNote], in: IIdxSeq[OffsetNote], out: IIdxSeq[OffsetNote]): (IIdxSeq[OffsetNote], IIdxSeq[OffsetNote]) =
      rem match {
        case head +: tail =>
          val minTime   = head.offset - timeTol
          val maxTime   = head.stop   + timeTol
          val minPitch  = head.pitch.midi - pitchTol
          val maxPitch  = head.pitch.midi + pitchTol
          val (in1, out1) = out.partition(inHeap(minTime, maxTime, minPitch, maxPitch))
          mkHeap(rem = tail ++ in1, in = in :+ head, out = out1)

        case _ => (in, out)
      }

    @tailrec def mkHeaps(rem: IIdxSeq[OffsetNote], res: IIdxSeq[IIdxSeq[OffsetNote]]): IIdxSeq[IIdxSeq[OffsetNote]] = {
      rem match {
        case head +: tail =>
          val (in1, out1) = mkHeap(rem = Vector(head), in = Vector.empty, out = tail)
          mkHeaps(out1, res :+ in1)
        case _ => res
      }
    }

    val heaps = mkHeaps(notes, Vector.empty)
    implicit val r  = sn.rate
    show(heaps, Vector.empty, numGroups = 4)
  }
}
