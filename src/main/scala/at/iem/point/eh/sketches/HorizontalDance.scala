//package at.iem.point.eh.sketches
//
//import at.iem.point.illism.OffsetNote
//import de.sciss.midi.TickRate
//
//object HorizontalDance {
//  def apply(): HorizontalDance = ???
//
//  private final class Impl(m: Vector[Vector[OffsetNote]]) extends HorizontalDance {
//    def move(n: Int): Vector[OffsetNote] = {
//      val notesIn   = m.flatten //  m.flatMap(_._2)
//      val pitchSq   = notesIn.map(_.pitch.midi)
//
//      val recPch    = ContextDance.move(pitchSq, num = NUM, seed = SEED)(pitchSq(START) :: Nil)
//
//      val notesOut0 = recPch.zipWithIndex.map { case (midi, idx) =>
//        val off = idx * 0.25
//        OffsetNote(off, midi.asPitch, 0.125, 80)
//      }
//
//      val notesOut1 = if (VELO) {
//        val veloSq  = notesIn.map { n => val v = n.velocity; v - (v % VELO_COARSE) }
//        val recVelo = ContextDance.move(veloSq, num = NUM, seed = SEED)(veloSq(START) :: Nil)
//        (notesOut0 zip recVelo).map { case (n, v) => n.copy(velocity = v) }
//
//      } else notesOut0
//
//      implicit val rate = TickRate.tempo(120, 1024)
//
//    //  val sorted = notesIn.sortBy(_.offset)
//    //  assert(notesIn == sorted)
//
//      val notesOut  = if (ENTRY) {
//        val entrySq = notesIn.sliding(2, 1).to[Vector].map { case Seq(a, b) =>
//          val fine    = ((b.offset - a.offset) * 1000 + 0.5).toInt // millis
//          val coarse  = (fine * ENTRY_COARSE + 0.5).toInt
//          if (coarse > 0) (fine - fine % coarse) else fine
//    //      millis * 1000
//        }
//
//    //    println(entrySq.mkString(", "))
//    //    ContextDance.DEBUG = true
//        val recEntry = ContextDance.move(entrySq :+ entrySq.head, num = NUM, seed = SEED)(entrySq(START) :: Nil)
//    //    println(recEntry.mkString(", "))
//        var off = 0.0
//        (notesOut1 zip recEntry).map { case (n, e) =>
//          val res = n.copy(offset = off)
//          off += e * 0.001 * ENTRY_SCALE
//          res
//        }
//
//      } else notesOut1
//    }
//  }
//}
//sealed trait HorizontalDance {
//  def move(n: Int): Vector[OffsetNote]
//}