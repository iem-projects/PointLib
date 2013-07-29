package at.iem.point.ot

import de.sciss.file._
import de.sciss.midi
import at.iem.point.illism._
import scala.annotation.tailrec

package object sketches {
  val Vec             = collection.immutable.IndexedSeq
  type Vec[+A]        = collection.immutable.IndexedSeq[A]

  val pointHome       = userHome / "Desktop" / "IEM" / "POINT"
  val baseDir         = pointHome / "composers" / "orestis_toufektsis"
  val materialDir     = baseDir / "material"
  val chordsSeqFile   = materialDir / "13-07-28-AKKORDSERIEN-orestis.mid"
  lazy val chordSeq   = midi.Sequence.readFile(chordsSeqFile)
  lazy val chords     = {
    val res = chordSeq.notes.splitGroups()
    assert(res.size == 7)
    res
  }

  implicit class OTRichNotes(val seq: Vec[OffsetNote]) extends AnyVal {
    def splitGroups(minimumPause: Double = 1.0): Vec[Vec[OffsetNote]] = {
      @tailrec def loop1(head: Vec[OffsetNote], tail: Vec[OffsetNote],
                         stop: Double): (Vec[OffsetNote], Vec[OffsetNote]) =
        tail match {
          case hd +: tl if (hd.offset < stop + minimumPause) =>
            val nextStop = math.max(stop, hd.stop)
            loop1(head :+ hd, tl, nextStop)
          case _ =>
            (head, tail)
        }

      @tailrec def loop(res: Vec[Vec[OffsetNote]], rem: Vec[OffsetNote]): Vec[Vec[OffsetNote]] =
        rem match {
          case hd +: tl =>
            val (prev, next) = loop1(Vec.empty, rem, hd.stop)
            loop(res :+ prev, next)
          case _ =>
            res
        }

      loop(Vec.empty, seq)
    }
  }
}
