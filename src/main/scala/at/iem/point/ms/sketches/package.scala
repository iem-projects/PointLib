package at.iem.point.ms

import java.awt.EventQueue
import java.text.DecimalFormat
import java.math.RoundingMode
import de.sciss.midi
import language.higherKinds
import de.sciss.file._

package object sketches {
  val  Vec    = collection.immutable.IndexedSeq
  type Vec[A] = collection.immutable.IndexedSeq[A]

  var recPath = userHome / "Desktop" / "IEM" / "POINT" / "composers" / "mattias_skoeld"

  object Study {
    case class Raw(idx: Int) extends Study {
      def raw = true
      def file: File = if (idx == 0) {
        recPath / "MIDI" / s"ms_midiexample_[raw].mid"
      } else {
        val base               = recPath / "MIDI3"
        val f0                 = base / f"study_#$idx%02d.mid"
        if (f0.exists()) f0 else base / f"study_#$idx%02d!.mid"
      }
    }

    case class Edited(idx: Int) extends Study {
      def raw = false
      def file: File = if (idx == 0) {
        recPath / "MIDI" / s"ms_midiexample_[edited].mid"
      } else {
        sys.error(s"No edited file for index $idx")
      }
    }

    case class Boring(idx: Int) extends Study {
      def raw = true
      def file: File = recPath / "boring" / f"study_#$idx%02du.mid"
    }
  }
  sealed trait Study {
    def file: File
    def idx: Int
    def raw: Boolean
  }

  def load(study: Study = Study.Edited(0)): midi.Sequence = {
    midi.Sequence.readFile(study.file)
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
}
