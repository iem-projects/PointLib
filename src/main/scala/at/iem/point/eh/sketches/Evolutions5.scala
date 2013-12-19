package at.iem.point.eh.sketches

import de.sciss.midi.{Sequencer, Sequence, Track, TickRate}
import at.iem.point.illism._
import de.sciss.file._
import scala.util.Try

object Evolutions5 extends App {
  // with start 0, seeds of 1, 2 creates funny loops; 0 and 3 have many walks, 4 is great because it keeps looping but then escapes
  val NUM         = 2000
  val SEED        = 5L      // seed of the random number generator
  val START       = 1       // start index in the pitch sequence to begin wih
  val VELO        = true    // model velocity
  val VELO_COARSE = 4       // velocity rasterisation (in steps)
  val ENTRY       = false // true    // model entry offsets
  val ENTRY_COARSE= 0.25     // entry offset rasterisation (relative, in percent 0...1)
  val ENTRY_SCALE = 2.0 // 1.5     // slow down factor if using entry modelling

  // val sq        = loadSnippet(improvSnippets.last)
  // val notesIn   = sq.notes.sortBy(_.offset)

  val notesIn: Vec[OffsetNote] = {
    val sqs = (0 to 4).map(loadDisklavier)
    val res = ((0.0, Vec.empty[OffsetNote]) /: sqs) { case ((offset, _res), sq) =>
      val ns  = sq.notes
      // assert(ns == ns.sortBy(_.offset))
      val d   = offset - ns.head.offset
      // println(f"Offset $d%1.2f")
      val newNotes  = ns.map(n => n.copy(offset = n.offset + d))
      val newOff    = newNotes.last.stop + newNotes.last.duration
      (newOff, _res ++ newNotes)
    } ._2
    println(s"Total number of notes: ${res.size}")
    res
  }

  val pitchSq2  = notesIn.map(n => (n.offset, n.pitch.keyColor, n.pitch.keyPosition))
  val ivalSq    = pitchSq2.pairMap {
    case ((o1, c1, p1), (o2, c2, p2)) =>
      val fine    = ((o2 - o1) * 1000 + 0.5).toInt // millis
      val coarse  = (fine * ENTRY_COARSE + 0.5).toInt
      val d       = if (coarse > 0) fine - fine % coarse else fine
      (d, c1, c2, p2 - p1)
  }

  // val pitchSq   = notesIn.map(_.pitch.midi)

  implicit val rnd  = new util.Random(SEED)
  // val recPch    = ContextDance.move(pitchSq, num = NUM)(pitchSq(START) :: Nil)
  val recPch    = ContextDance.move(ivalSq, num = NUM)(ivalSq(START) :: Nil)

//  println(recPch.map(_.asPitch).mkString(", "))

  val startPitch: Pitch = recPch.head match {
    case (dur, c1, c2, dp) => (60 until 72).map(_.asPitch).filter { pch =>
      Try(pch.keyColor == c1 && pch.moveBy(dp).keyColor == c2).getOrElse(false)
    } .head
  }

  def random1() = if (rnd.nextBoolean()) 1 else -1

  val notesOut0 = ((startPitch, 0, Vec.empty[OffsetNote]) /: recPch) { case ((pch, off, res), (dur, c1, c2, dp)) =>
    // val idx   = res.size
    // val off   = idx * 0.25
    val res1  = res :+ OffsetNote(off * 0.001, pch, 0.125, 80)
    val pch1  = Try(pch.moveBy(dp)).getOrElse(pch.moveBy(dp + random1()))

    val pch2 = if (pch1.keyColor != c2) {
      val m = if (pch1.keyColor == KeyColor.Black)
        random1()
      else
        pch1.`class`.step match {
          case 0 |  5  => 1
          case 4 | 11  => -1
          case _       => random1()
        }
      new Pitch(pch1.midi + m)
    } else {
      pch1
    }

    // assert(pch.keyColor == c1 && pch1.keyColor == c2)
    val off1 = off + dur
    (pch2, off1, res1)
  } ._3

  val notesOut1 = if (VELO) {
    val veloSq  = notesIn.map { n => val v = n.velocity; v - (v % VELO_COARSE) }
    val recVelo = ContextDance.move(veloSq, num = NUM)(veloSq(START) :: Nil)
    (notesOut0 zip recVelo).map { case (n, v) => n.copy(velocity = v) }

  } else notesOut0

  implicit val rate = TickRate.tempo(120, 1024)

//  val sorted = notesIn.sortBy(_.offset)
//  assert(notesIn == sorted)

  val notesOut  = if (ENTRY) {
    val entrySq = notesIn.sliding(2, 1).to[Vector].map { case Vec(a, b) =>
      val fine    = ((b.offset - a.offset) * 1000 + 0.5).toInt // millis
      val coarse  = (fine * ENTRY_COARSE + 0.5).toInt
      if (coarse > 0) fine - fine % coarse else fine
//      millis * 1000
    }

//    println(entrySq.mkString(", "))
//    ContextDance.DEBUG = true
    val recEntry = ContextDance.move(entrySq :+ entrySq.head, num = NUM)(entrySq(START) :: Nil)
//    println(recEntry.mkString(", "))
    var off = 0.0
    (notesOut1 zip recEntry).map { case (n, e) =>
      val res = n.copy(offset = off)
      off += e * 0.001 * ENTRY_SCALE
      res
    }

  } else notesOut1

  val events  = notesOut.flatMap(_.toMIDI)
  val track   = Track(events)
  val sqOut   = Sequence(Vector(track))

  sqOut.writeFile(outPath / s"DisklavierBW_PitchSeq_${START}_${SEED}${if (VELO) "V" else ""}${if (ENTRY) "E" else ""}.mid")

  val player  = Sequencer.open()
  player.play(sqOut)
  Thread.sleep(((track.duration + 1) * 1000).toLong)
  player.stop()
  player.close()
}