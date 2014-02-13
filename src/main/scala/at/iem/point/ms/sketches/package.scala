package at.iem.point.ms

import java.awt.EventQueue
import java.text.DecimalFormat
import java.math.RoundingMode
import de.sciss.midi
import language.higherKinds
import de.sciss.file._
import scala.util.control.NonFatal

package object sketches {
  val  Vec    = collection.immutable.IndexedSeq
  type Vec[A] = collection.immutable.IndexedSeq[A]

  var recPath = userHome / "Desktop" / "IEM" / "POINT" / "composers" / "mattias_skoeld"

  object Study {
    case class Raw(idx: Int) extends Study {
      def isRaw     = true
      def isBoring  = false
      def file: File = if (idx == 0) {
        recPath / "MIDI" / s"ms_midiexample_[raw].mid"
      } else {
        val base               = recPath / "MIDI3"
        val f0                 = base / f"study_#$idx%02d.mid"
        if (f0.exists()) f0 else base / f"study_#$idx%02d!.mid"
      }
    }

    case class Edited(idx: Int) extends Study {
      def isRaw     = false
      def isBoring  = false
      def file: File = if (idx == 0) {
        recPath / "MIDI" / s"ms_midiexample_[edited].mid"
      } else {
        sys.error(s"No edited file for index $idx")
      }
    }

    case class Boring(idx: Int) extends Study {
      def isRaw     = true
      def isBoring  = true
      def file      = recPath / "boring" / f"study_#$idx%02du.mid"
    }

    case class Promising(idx: Int) extends Study {
      def isRaw     = true
      def isBoring  = false
      def file      = recPath / "promising" / f"study_#$idx%02d!.mid"
    }
  }
  sealed trait Study {
    def file    : File
    def idx     : Int
    def isRaw   : Boolean
    def isBoring: Boolean
  }

  def load(study: Study = Study.Edited(0)): midi.Sequence = try {
    midi.Sequence.readFile(study.file)
  } catch {
    case NonFatal(e) =>
      Console.err.println(s"In '${study.file}':")
      throw e
  }

  def newBoring     = allBoring   .filterNot(s => Vec(26, 29, 31).contains(s.idx))
  def newPromising  = allPromising.filterNot(s => Vec( 5, 10    ).contains(s.idx))

  private def studyIndices(name: String, delim: Char): Vec[Int] = (recPath / name).children(_.ext == "mid").map { f =>
    val b  = f.base
    val i  = b.indexOf('#') + 1
    val j  = b.indexOf(delim, i)
    b.substring(i, j).toInt
  }

  def allBoring     = studyIndices("boring"   , 'u').map(Study.Boring   )
  def allPromising  = studyIndices("promising", '!').map(Study.Promising)

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
}