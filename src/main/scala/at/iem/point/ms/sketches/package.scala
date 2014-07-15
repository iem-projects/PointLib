/*
 *  package.scala
 *  (PointLib - ms)
 *
 *  Copyright (c) 2013-2014 IEM Graz / Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

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

  private val desktop = userHome / "Desktop"
  private val baseDir = if ((desktop / "IEM").exists) desktop else userHome
  var recPath = baseDir / "IEM" / "POINT" / "composers" / "mattias_skoeld"

  require(recPath.exists, s"Cannot find base directory '$recPath'")

  object Study {
    def apply(idx: Int, isBoring: Boolean, file: File, isRaw: Boolean = false): Study =
      Apply(idx, isBoring, file, isRaw)

    private case class Apply(idx: Int, isBoring: Boolean, file: File, isRaw: Boolean) extends Study

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

    case class Blind(file: File) extends StudyLike
  }
  trait StudyLike {
    def file    : File
  }
  sealed trait Study extends StudyLike {
    def idx     : Int
    def isRaw   : Boolean
    def isBoring: Boolean
  }

  def load(study: StudyLike = Study.Edited(0)): midi.Sequence = try {
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
  } .sorted

  def allBoring     = studyIndices("boring"   , 'u').map(Study.Boring   )
  def allPromising  = studyIndices("promising", '!').map(Study.Promising)

  def allBlind      = (recPath / "blind").children(_.ext == "mid").sortBy(_.name).map(Study.Blind)

  def defer(thunk: => Unit): Unit =
    if (EventQueue.isDispatchThread) thunk else EventQueue.invokeLater(new Runnable { def run(): Unit = thunk })

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
