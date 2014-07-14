/*
 *  OtherFeatures.scala
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

package at.iem.point.ms.sketches

import at.iem.point.illism._

object OtherFeatures {
  def horizPitchClasses(study: StudyLike): Vec[Double] = {
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
