package at.iem.point.ot

import de.sciss.file._
import de.sciss.midi
import at.iem.point.illism._
import scala.annotation.tailrec
import language.higherKinds

package object sketches {
  val Vec             = collection.immutable.IndexedSeq
  type Vec[+A]        = collection.immutable.IndexedSeq[A]

  val pointHome       = userHome / "Desktop" / "IEM" / "POINT"
  val baseDir         = pointHome / "composers" / "orestis_toufektsis"
  val materialDir     = baseDir / "material"
  val outDir          = baseDir / "out"

  val chordsSeqFile   = materialDir / "13-07-28-AKKORDSERIEN-orestis.mid"
  lazy val chordSeq   = midi.Sequence.readFile(chordsSeqFile)
  lazy val chords     = {
    val groups = chordSeq.notes.splitGroups()
    assert(groups.size == 7)
    val _chords = groups.map(n => ChordUtil.findHarmonicFields(n))  // .findChords(n))
    _chords.zipWithIndex.foreach { case (c, i) =>
      val sz = chordVoices(i)
      assert(c.forall(_.size == sz), s"In group ${i + 1}, chord size is not $sz")
    }
    _chords
  }

  def chordVoices(idx: Int) = idx + 1 match {
    case 1  => 3
    case 4  => 4
    case 6  => 6
    case 2 | 3 | 5 | 7 => 5
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

  implicit class OTRichChords(val seq: Vec[Chord]) extends AnyVal {
    def spread(spacing: Double = 0.0): Vec[Chord] =
      seq match {
        case head +: tail =>
          tail.scanLeft(head) { (pred, succ) =>
            succ.copy(notes = succ.notes.map(_.copy(offset = pred.maxStop + spacing)))
          }
        case _ => seq
      }
  }

  implicit final class RichIndexedSeq[A](val seq: IndexedSeq[A]) extends AnyVal {
    def choose(implicit random: util.Random): A = seq(random.nextInt(seq.size))
    def scramble(implicit random: util.Random): Vec[A] = {
      ((seq, Vector.empty[A]) /: (0 until seq.size)) { case ((in, out), _) =>
        val idx = random.nextInt(in.size)
        in.patch(idx, Vector.empty, 1) -> (out :+ in(idx))
      } ._2
    }

    def integrate(implicit num: Numeric[A]): Vec[A] = {
      (Vector.empty[A] /: seq) { (res, elem) =>
        val agg = num.plus(res.lastOption.getOrElse(num.zero), elem)
        res :+ agg
      }
    }
  }

  // implicit val random = new util.Random(0L)

  def mkRandom(seed: Long = System.currentTimeMillis()) = new util.Random(seed)

  //  implicit class OTRichSeq[A, C[~] <: Iterable[~]](val seq: C[A]) extends AnyVal {
  //    def mapType[B](implicit view: A => B): C[B] = seq.map(view)
  //  }
  //
  //  class FilterMapImpl[A, Repr](val r: GenTraversableLike[A, Repr]) {
  //    def filterMap[B, That](f: A => Option[B])(implicit cbf : CanBuildFrom[Repr, B, That]): That =
  //      r.flatMap(f(_).toSeq)
  //  }
  //
  //  implicit def filterMap[A, Repr](r: Repr)(implicit fr: IsTraversableLike[Repr]): FilterMapImpl[fr.A, Repr] =
  //    new FilterMapImpl(fr conversion r)
}
