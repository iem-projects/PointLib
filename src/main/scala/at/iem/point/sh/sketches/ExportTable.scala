package at.iem.point.sh.sketches

import Fitness._
import at.iem.point.sh.sketches.gui.ChromosomeView
import de.sciss.pdflitz.Generate
import java.awt.Dimension
import de.sciss.desktop.{FileDialog, Window}
import de.sciss.file._
import scala.annotation.tailrec

object ExportTable {
  private def defaultFile(): File = {
    val desktop = userHome / "Desktop"
    val dir     = if (desktop.canWrite) desktop else userHome

    @tailrec def loop(i: Int): File = {
      val test = dir / s"out${if (i == 0) "" else (i+1).toString}.pdf"
      if (test.exists()) loop(i + 1) else test
    }

    loop(0)
  }

  def dialog(genome: GenomeVal, parent: Option[Window] = None) {
    val dlg = FileDialog.save(init = Some(defaultFile()), title = "Export Selection As PDF Table")
    val res = dlg.show(parent)
    res.foreach { f =>
      ExportTable(genome = genome, out = f)
    }
  }

  def apply(genome: GenomeVal, out: File): Unit = {
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

    Generate(out, view, overwrite = false)
  }
}
