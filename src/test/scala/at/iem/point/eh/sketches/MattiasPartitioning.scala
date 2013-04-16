package at.iem.point.eh.sketches

import scala.swing.Swing
import at.iem.point.illism._
import de.sciss.midi

object MattiasPartitioning extends App with ShowPartitioning {
  Swing.onEDT(run())

  def fields        = true
  def splitVoices   = false && !fields
  def studyIdx      = 1

  def run() {
    val i           = 2
    val sn          = midi.Sequence.read(f"/Users/hhrutz/Desktop/IEM/POINT/composers/mattias_skoeld/MIDI3/study_#$studyIdx%02d.mid")
    val chans       = if (splitVoices) 0 until 4 else Vector(-1)
    val data        = if (fields) {
      val notes     = sn.notes
      val _h        = ChordUtil.findHarmonicFields(notes)
      Vector(Vector.empty -> _h)

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
