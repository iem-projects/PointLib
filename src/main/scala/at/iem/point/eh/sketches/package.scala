package at.iem.point.eh

import java.io.{IOException, File}
import java.awt.EventQueue
import java.text.DecimalFormat
import java.math.RoundingMode
import collection.{mutable, breakOut}

package object sketches {
  val  IIdxSeq    = collection.immutable.IndexedSeq
  type IIdxSeq[A] = collection.immutable.IndexedSeq[A]

  def file(path: String): File = new File(path)

  implicit final class RichFile(val f: File) extends AnyVal {
    def / (child: String): File = new File(f, child)
    def files: List[File] = {
      val arr = f.listFiles()
      if (arr == null) throw new IOException(s"Not a directory: ${f}")
      arr.toList
    }
    def filesOption: Option[List[File]] = Option(f.listFiles()).map(_.toList)
    def name: String = f.getName
  }

  var recPath = file(sys.props("user.home")) / "Desktop" / "IEM" / "POINT" / "composers" / "elisabeth_harnik"
  lazy val snippetFiles: Map[Int, File] = {
    val b   = Map.newBuilder[Int, File]
    val Pat = "snippet (\\d+).mid".r
    def loop(d: File) {
      d.filesOption.getOrElse(Nil).foreach { f =>
        if (f.isFile) f.name match {
          case Pat(num) => b += num.toInt -> f
          case _ =>
        } else loop(f)
      }
    }
    loop(recPath)
    b.result()
  }

  def loadSnippet(idx: Int): midi.Sequence = midi.Sequence.read(snippetFiles(idx))

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

  implicit final class RichInt(val i: Int) extends AnyVal {
    def asPitch: Pitch = new Pitch(i)
  }

  implicit final class RichIterable[A](val it: Iterable[A]) extends AnyVal {
//    def histogram(implicit ord: Numeric[A]): Map[A, Int] = histogram(ord.zero)
//    def histogram(tolerance: A)(implicit ord: Numeric[A]): Map[A, Int] = {
//      val b = Map.newBuilder[A, Int]
//      @tailrec def loop(xs: IIdxSeq[A]) {
//        if (xs.isEmpty) return
//        val h = xs.head
//        val (bin)
//      }
//      b.result()
//    }

    def intervals(implicit ev: A <:< Pitch): IIdxSeq[DirectedInterval] =
      it.sliding(2,1).map({ case Seq(low, high) => high interval low }).toIndexedSeq

    def isSortedBy[B](fun: A => B)(implicit ord: Ordering[B]): Boolean = {
      it.sliding(2, 1).forall { case Seq(a, b) => ord.lteq(fun(a), fun(b)) }
    }

    def histogram: Map[A, Int] = {
      var res = Map.empty[A, Int] withDefaultValue 0
      it.foreach { elem =>
        res += elem -> (res(elem) + 1)
      }
      res
    }
  }

  def defer(thunk: => Unit) {
    if (EventQueue.isDispatchThread) thunk else EventQueue.invokeLater(new Runnable { def run() { thunk }})
  }

//  final val german  = Language.German
//  final val english = Language.English

  private lazy val dfRound3 = {
    val res = new DecimalFormat("#.###")
    res.setRoundingMode(RoundingMode.HALF_UP)
    res
  }

  implicit final class RichSeconds(val sec: Double) extends AnyVal {
    def roundSecondsToMillis: String = dfRound3.format(sec)
  }

  implicit final class RichSequence(val sq: midi.Sequence) extends AnyVal {
    def notes: IIdxSeq[OffsetNote] = sq.tracks.flatMap(_.notes)
  }

  implicit final class RichTrack(val t: midi.Track) extends AnyVal {
    def notes: IIdxSeq[OffsetNote] = {
      val r     = t.sequence.tickRate
      val b     = IIdxSeq.newBuilder[OffsetNote]
      val wait  = mutable.Map.empty[(Int, Int), (Double, midi.NoteOn)]
      t.events.foreach {
        case midi.Event(tick, on @ midi.NoteOn(ch, pitch, _)) =>
          val startTime = tick / r.ticksPerSecond
          wait += (ch, pitch) -> (startTime, on)

        case midi.Event(tick, off @ midi.NoteOff(ch, pitch, _)) =>
          val stopTime  = tick / r.ticksPerSecond
          wait.remove(ch -> pitch).foreach { case (startTime, on) =>
            b += OffsetNote(offset = startTime, /* channel = ch, */ pitch = pitch.asPitch, duration = stopTime - startTime,
              velocity = on.velocity /*, release = off.velocity */)
          }

        case _ =>
      }
      if (wait.nonEmpty) {
        println(s"Warning: pending notes ${wait.mkString("(", ",", ")")}")
      }
      b.result()
    }
  }
}
