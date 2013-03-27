package at.iem.point.eh

import java.io.{IOException, File}
import java.awt.EventQueue
import de.sciss.midi

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
}
