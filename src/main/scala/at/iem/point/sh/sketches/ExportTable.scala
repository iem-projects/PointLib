package at.iem.point.sh.sketches

import Fitness._
import java.io.File
import at.iem.point.sh.sketches.genetic.Settings
import at.iem.point.sh.sketches.gui.ChromosomeView
import de.sciss.pdflitz.Generate
import java.awt.Dimension

object ExportTable {
  def apply(f: File, genome: GenomeVal, settings: Settings) {
    val num   = genome.size
    // val dur   = settings.generation.wholeDur.toDouble * 1.1
    val dur   = genome.map(_._1.dur).max.toDouble
    val cw    = math.ceil(ChromosomeView.preferredScale * dur).toInt
    val yGap  = 4
    val ch0   = ChromosomeView.preferredHeight
    val ch1   = ch0 + yGap
    val ch    = num * ch1 - yGap

    val view  = Generate.QuickDraw(new Dimension(cw, ch)) { g =>
      val atOrig = g.getTransform
      genome.zipWithIndex.foreach { case ((cn, fit), idx) =>
        try {
          g.translate(0, idx * ch1)
          ChromosomeView.paint(cn.map(_.normalized), g, cw, ch0, dur)
        } finally {
          g.setTransform(atOrig)
        }
      }
    }

    Generate(f, view, overwrite = false)
  }
}
