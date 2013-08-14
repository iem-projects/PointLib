package at.iem.point.eh.sketches

import scala.swing.Swing
import de.sciss.file._
import de.sciss.midi
import at.iem.point.illism._

object MattiasPartitioning extends App with ShowPartitioning {
  Swing.onEDT(run())

  def fields        = false // true
  def splitVoices   = /* true && */ !fields
  // def studyIndices  = 26
  // def boring        = true

  def files         = Seq(26 -> true, 5 -> false)

  def run(): Unit =
    files.foreach { case (idx, b) =>
      show(idx, boring =b)
    }

  def show(studyIdx: Int, boring: Boolean): Unit = {
    val base        = "/Users/hhrutz/Desktop/IEM/POINT/composers/mattias_skoeld/"
    val base1       = if (boring) s"$base/boring/" else s"$base/MIDI3/"
    val path0       = f"$base1/study_#$studyIdx%02d${if (boring) "u" else ""}.mid"
    val path        = if (file(path0).exists()) path0 else path0.dropRight(4) + "!.mid"
    val sn          = midi.Sequence.read(path)
    val chans       = if (splitVoices) 0 until 4 else Vec(-1)
    val data        = if (fields) {
      val notes     = sn.notes
      val _h        = ChordUtil.findHarmonicFields(notes)
      Vec(Vec.empty -> _h)

    } else {
      chans.map { ch =>
        val notes     = sn.notes(channel = ch)
        val (_m, _h)  = NoteUtil.splitMelodicHarmonic(notes)
        val nm        = _m.flatMap(_._2)
        val nh        = _h.flatMap(_._2)
        println(s"In channel $ch there are ${notes.size} notes, partitioned as ${nm.size} melodic and ${nh.size} harmonic entities")
        nm -> nh
      }
    }
    val (m, h) = data.unzip

    implicit val r  = sn.rate
    show(m, h)
  }
}
