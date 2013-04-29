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
    val sn    = loadDisklavier(5) // 4  1  0
    val notes = sn.notes

    val timeTol   = 0.5 // 0.3
    val pitchTol  = 4.0 // 3.0 // Double!
    // val diagTol   = math.sqrt(timeTol * timeTol + pitchTol * pitchTol)

    def inHeap(ref: OffsetNote)(n: OffsetNote): Boolean = {
      val np  = n.pitch.midi
      val rp  = ref.pitch.midi
      val pch = math.abs(np - rp)

      // n.offset < maxTime && n.stop > minTime && p >= minPitch && p <= maxPitch

      if (n.offset < ref.offset) {
        // note begins before ref

        if (n.stop >= ref.offset) {
          // notes touch or overlap in time
          pch <= pitchTol

        } else {
          // n precedes ref in time
          val dx      = (ref.offset - n.stop) / timeTol
          val dy      = math.max(0, pch - 1) / pitchTol
          val distSq  = dx * dx + dy * dy
          distSq <= 1.0
        }

      } else {
        // note begins after ref

        if (n.offset <= ref.stop) {
          // notes touch or overlap in time
          pch <= pitchTol

        } else {
          // n succeeds ref in time
          val dx      = (n.offset - ref.stop) / timeTol
          val dy      = math.max(0, pch - 1) / pitchTol
          val distSq  = dx * dx + dy * dy
          distSq <= 1.0
        }
      }
    }

    def mkHeap(rem: IIdxSeq[OffsetNote], in: IIdxSeq[OffsetNote], out: IIdxSeq[OffsetNote]): (IIdxSeq[OffsetNote], IIdxSeq[OffsetNote]) =
      rem match {
        case head +: tail =>
          val (in1, out1) = out.partition(inHeap(head))
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
    show(heaps, Vector.empty, numGroups = 5)
  }
}
