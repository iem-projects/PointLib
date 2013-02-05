package at.iem.point.eh

import java.io.{IOException, File}
import annotation.tailrec
import java.awt.EventQueue

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
}
