package at.iem.point.eh

import java.awt.EventQueue
import de.sciss.midi
import at.iem.point.illism.{PitchClass, Pitch, Chord}
import de.sciss.file._
import scala.collection.generic.CanBuildFrom
import language.higherKinds

package object sketches {
  val  Vec    = collection.immutable.IndexedSeq
  type Vec[A] = collection.immutable.IndexedSeq[A]

  implicit final class RichFloat(val f: Float) extends AnyVal {
    def linlin(srcLo: Float, srcHi: Float, dstLo: Float, dstHi: Float): Float =
      (f - srcLo) / (srcHi - srcLo) * (dstHi - dstLo) + dstLo

    def linexp(srcLo: Float, srcHi: Float, dstLo: Float, dstHi: Float): Float =
      math.pow(dstHi / dstLo, (f - srcLo) / (srcHi - srcLo)).toFloat * dstLo
  }

  implicit final class RichDouble(val d: Double) extends AnyVal {
    def linlin(srcLo: Double, srcHi: Double, dstLo: Double, dstHi: Double): Double =
      (d - srcLo) / (srcHi - srcLo) * (dstHi - dstLo) + dstLo

    def linexp(srcLo: Double, srcHi: Double, dstLo: Double, dstHi: Double): Double =
      math.pow(dstHi / dstLo, (d - srcLo) / (srcHi - srcLo)).toFloat * dstLo
  }

  var basePath  = file(sys.props("user.home")) / "Desktop" / "IEM" / "POINT" / "composers" / "elisabeth_harnik"
  def inPath    = basePath / "in"
  def outPath   = basePath / "rec"
  def outPath2  = basePath / "rec_neu"

  lazy val snippetFiles: Map[Int, File] = {
    val b   = Map.newBuilder[Int, File]
    val Pat = "snippet (\\d+).mid".r
    def loop(d: File): Unit =
      d.children.foreach { f =>
        if (f.isFile) f.name match {
          case Pat(num) => b += num.toInt -> f
          case _ =>
        } else loop(f)
      }
    loop(inPath)
    b.result()
  }

  def loadSnippet(idx: Int): midi.Sequence = midi.Sequence.read(snippetFiles(idx).getPath)

  private val disklavierNames = Vector(
    "Kreisend", "Cluster", "StummeTasten", "4_TeilweisePedal", "MitGanzOberenHoehen",
    "SchleifenderDaumen"
  )

  /** Loads a MIDI snippet from the disklavier session.
    *
    * @param idx  file index from 0 to (including) 4
    */
  def loadDisklavier(idx: Int): midi.Sequence = midi.Sequence.read((outPath2 / s"${disklavierNames(idx)}.mid").getPath)

  // maps voices to snippet indices
  lazy val staticChords = Map(
    2 -> List(11, 12, 13),
    3 -> List(14, 15, 16),
    4 -> List(18, 19, 20),
    5 -> List(46),
    6 -> List(21, 22, 23)
  )

  // snippet indices of free improvisation sets
  lazy val improvSnippets = 9 :: 48 :: Nil

  def defer(thunk: => Unit): Unit =
    if (EventQueue.isDispatchThread) thunk else EventQueue.invokeLater(new Runnable { def run() { thunk }})

  implicit final class RichChord(val chord: Chord) extends AnyVal {
     def avgVelocity: Float = {
       val v = chord.notes.map(_.velocity).sum
       v.toFloat / chord.size
     }
  }

  object KeyColor {
    case object Black extends KeyColor
    case object White extends KeyColor
  }
  sealed trait KeyColor

  def keyPositionToPitch(pos: Int): Pitch = {
    val pos1  = if (pos < 25 /* 0 */) {
      println("Warning: moving too low  pitch back into MIDI register")
      val add = (25 - pos + 13) / 14 * 14
      pos + add
    } else if (pos > 126 /* 148 */) {
      println("Warning: moving too high pitch back into MIDI register")
      val sub = (pos - 126 /* 148 */ + 13) / 14 * 14
      pos - sub
    } else {
      pos
    }
    val oct   = pos1 / 14
    val pos2  = pos1 % 14
    require(pos2 != 5 && pos2 != 13, "Ended up at illegal key position")
    val step  = if (pos2 <= 4) pos2 else pos2 - 1

    new Pitch(oct * 12 + step)
  }

  implicit final class RichPitchClass(/* val */ p: PitchClass) /* extends AnyVal */ {
    def keyColor: KeyColor = p.step match {
      case 1 | 3 | 6 | 8 | 10 => KeyColor.Black
      case _                  => KeyColor.White
    }

    def keyPosition: Int = if (p.step <= 4) p.step else p.step + 1
  }

  implicit final class RichPitch(/* val */ pitch: Pitch) /* extends AnyVal */ {
    def keyColor: KeyColor = pitch.`class`.keyColor

    def keyPosition: Int = {
      val oct = pitch.midi / 12
      oct * 14 + pitch.`class`.keyPosition
    }

    /** A white key to another neighbouring white key is distance two,
      * transition between neighbouring white and black keys is distance one.
      */
    def distanceTo(that: Pitch): Int = that.keyPosition - this.keyPosition

    def moveBy(distance: Int): Pitch = {
      val pos0  = this.keyPosition + distance
      keyPositionToPitch(pos0)
    }
  }

  implicit final class RichEHIterableLike[A, CC[~] <: Iterable[~]](val it: CC[A]) extends AnyVal {
    def pairMap[B, To](fun: (A, A) => B)(implicit cbf: CanBuildFrom[CC[A], B, To]): To = {
      val b     = cbf(it)
      val iter  = it.iterator
      if (iter.hasNext) {
        var pred = iter.next()
        while (iter.hasNext) {
          val succ = iter.next()
          b   += fun(pred, succ)
          pred = succ
        }
      }
      b.result()
    }
  }

  implicit final class RichEHIndexedSeq[A](val seq: IndexedSeq[A]) extends AnyVal {
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
}
