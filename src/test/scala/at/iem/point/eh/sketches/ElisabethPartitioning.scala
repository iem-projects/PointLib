package at.iem.point.eh.sketches

import scala.swing.Swing
import at.iem.point.illism._
import scala.annotation.tailrec
import collection.breakOut
import de.sciss.numbers.Implicits._

object ElisabethPartitioning extends App with ShowPartitioning {
  Swing.onEDT(test1())
  // Swing.onEDT(test2())

  def test1(): Unit = {
    // val sn          = loadSnippet(improvSnippets(1))
    val sn          = loadFirstTests(s"test-12_1min_7.mid")
    val notes       = sn.notes
    val (m, h)      = NoteUtil.splitMelodicHarmonic(notes)
    val nm          = m.flatMap(_._2)
    val nh          = h.flatMap(_._2)

    implicit val r  = sn.rate
    show(Vec(nm), Vec(nh))
  }

  def test2(): Unit = {
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
    show(Vec(white, black), Vec.empty)
  }

  def run(): Unit = {
    val sn    = loadDisklavier(0) // 5  4  1  0
    val notes = sn.notes

    val timeTol       = 0.3 // 5 // 0.3           // seconds
    val pitchTol      = 4.0 // 3.0 // Double!     // semitones
    val densityWindow = 4.0                       // seconds

    // val diagTol   = math.sqrt(timeTol * timeTol + pitchTol * pitchTol)

    def calcDensity(window: Double)(n: OffsetNote): Double = {
      val minTime = n.offset - window
      val maxTime = n.stop   + window
      val num     = notes.count(that => that.offset >= minTime && that.stop <= maxTime)
      num / window
    }

    val densityFun = calcDensity(densityWindow) _
    val densityMap: Map[OffsetNote, Double] = notes.map(n => n -> densityFun(n))(breakOut)

    def inHeap(ref: OffsetNote)(n: OffsetNote): Boolean = {
      val np  = n.pitch.midi
      val rp  = ref.pitch.midi
      val pch = math.abs(np - rp)

      // n.offset < maxTime && n.stop > minTime && p >= minPitch && p <= maxPitch

      // val densityBonus = densityMap(n)

      if ((n.offset < ref.offset &&   n.stop >= ref.offset) ||
          (n.offset > ref.offset && ref.stop >= n  .offset)) {

        val durDif0   = math.max(n.duration, ref.duration) / math.min(n.duration, ref.duration)
        // val durDif    = math.min(3.0, durDif0)
        // val timeBonus = durDif.linexp(1.0, 2.0, 0.5, 2.0) // or linlin?
        val durDif    = math.min(5.0, durDif0)
        val timeBonus0 = durDif.linlin(1.0, 2.0, 0.5, 2.0)  // bonus for having similar duration

        val timeBonus1 = if (durDif0 < 1.4) {
          val ovrMin = math.min(n.stop, ref.stop) - math.max(n.offset, ref.offset)
          val ovrMax = math.max(n.stop, ref.stop) - math.min(n.offset, ref.offset)
          if (ovrMax / ovrMin < 1.5) {  // 'chord'
            0.25 // bonus for being simultaneous
          } else {
            1.0
          }
        } else 1.0

        val timeBonus = math.min(timeBonus0, timeBonus1)

        // notes touch or overlap in time
        pch * timeBonus <= pitchTol

      } else {
        // n precedes or succeeds ref in time
        val pitchBonus = if (pch == 0) 0.5 else 1.0
        val dx      = math.min(math.abs(n.offset - ref.stop), math.abs(ref.offset - n.stop)) / timeTol * pitchBonus
        val dy      = math.max(0, pch - 1) / pitchTol
        val distSq  = dx * dx + dy * dy
        distSq <= 1.0
      }
    }

    def mkHeap(rem: Vec[OffsetNote], in: Vec[OffsetNote], out: Vec[OffsetNote]): (Vec[OffsetNote], Vec[OffsetNote]) =
      rem match {
        case head +: tail =>
          val (in1, out1) = out.partition(inHeap(head))
          mkHeap(rem = tail ++ in1, in = in :+ head, out = out1)

        case _ => (in, out)
      }

    @tailrec def mkHeaps(rem: Vec[OffsetNote], res: Vec[Vec[OffsetNote]]): Vec[Vec[OffsetNote]] = {
      rem match {
        case head +: tail =>
          val (in1, out1) = mkHeap(rem = Vec(head), in = Vec.empty, out = tail)
          mkHeaps(out1, res :+ in1)
        case _ => res
      }
    }

    val heaps = mkHeaps(notes, Vec.empty)
    implicit val r  = sn.rate
    show(heaps, Vec.empty, numGroups = 5)
  }
}
