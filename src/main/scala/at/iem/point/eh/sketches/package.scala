package at.iem.point.eh

import java.io.{IOException, File}
import java.awt.EventQueue
import de.sciss.midi
import at.iem.point.illism.Chord

package object sketches {
  val  IIdxSeq    = collection.immutable.IndexedSeq
  type IIdxSeq[A] = collection.immutable.IndexedSeq[A]

  def file(path: String): File = new File(path)

  implicit final class RichFile(val f: File) extends AnyVal {
    def / (child: String): File = new File(f, child)
    def files: List[File] = {
      val arr = f.listFiles()
      if (arr == null) throw new IOException(s"Not a directory: $f")
      arr.toList
    }
    def filesOption: Option[List[File]] = Option(f.listFiles()).map(_.toList)
    def name: String = f.getName
  }

  implicit final class RichFloat(val f: Float) extends AnyVal {
    def linlin(srcLo: Float, srcHi: Float, dstLo: Float, dstHi: Float): Float =
      (f - srcLo) / (srcHi - srcLo) * (dstHi - dstLo) + dstLo

    def linexp(srcLo: Float, srcHi: Float, dstLo: Float, dstHi: Float): Float =
      math.pow(dstHi / dstLo, (f - srcLo) / (srcHi - srcLo)).toFloat * dstLo
  }

  var basePath  = file(sys.props("user.home")) / "Desktop" / "IEM" / "POINT" / "composers" / "elisabeth_harnik"
  def inPath    = basePath / "in"
  def outPath   = basePath / "rec"
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
    loop(inPath)
    b.result()
  }

  def loadSnippet(idx: Int): midi.Sequence = midi.Sequence.read(snippetFiles(idx).getPath)

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

  def defer(thunk: => Unit) {
    if (EventQueue.isDispatchThread) thunk else EventQueue.invokeLater(new Runnable { def run() { thunk }})
  }

  implicit final class RichChord(val chord: Chord) extends AnyVal {
     def avgVelocity: Float = {
       val v = chord.notes.map(_.velocity).sum
       v.toFloat / chord.size
     }
  }

  implicit final class RichIndexedSeq[A](val seq: IndexedSeq[A]) extends AnyVal {
    def choose(implicit random: util.Random): A = seq(random.nextInt(seq.size))
    def scramble(implicit random: util.Random): IIdxSeq[A] = {
      ((seq, Vector.empty[A]) /: (0 until seq.size)) { case ((in, out), _) =>
        val idx = random.nextInt(in.size)
        in.patch(idx, Vector.empty, 1) -> (out :+ in(idx))
      } ._2
    }

    def integrate(implicit num: Numeric[A]): IIdxSeq[A] = {
      (Vector.empty[A] /: seq) { (res, elem) =>
        val agg = num.plus(res.lastOption.getOrElse(num.zero), elem)
        res :+ agg
      }
    }
  }
}
