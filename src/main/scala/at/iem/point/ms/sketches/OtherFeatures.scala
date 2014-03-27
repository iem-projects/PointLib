package at.iem.point.ms.sketches

import at.iem.point.illism._

object OtherFeatures {
  def horizPitchClasses(study: Study): Vec[Double] = {
    val midi  = load(study)
    val histos = for (ch <- 0 until 4) yield {
      val notes: Vec[OffsetNote] = midi.notes(ch)
      val pch = notes.map(_.pitch.`class`.step)
      val h   = pch.histogram
      (0 until 12).map(h.apply)
    }
    val flat = histos.flatten
    val mx   = flat.max
    flat.map(_.toDouble / mx)
  }
}
