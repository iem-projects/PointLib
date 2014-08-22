package at.iem.point.er.sketches

import de.sciss.pdflitz
import de.sciss.desktop.FileDialog
import de.sciss.file._
import de.sciss.sonogram.Overview
import de.sciss.numbers.Implicits._

object PDFApp extends App {
  Main.main(null)
  Main.palette = x => Overview.Palette.Gray(x.max(0f).linlin(0, 1, 0.08f, 1)) // min. 8% gray
  Main.pdfFun = { frame =>
    val dlg = FileDialog.save(title = "Export Screenshot As PDF")
    dlg.show(Some(frame)).foreach { f =>
      pdflitz.Generate(f.replaceExt("pdf"), frame.view.printComponent, overwrite = true)
    }
  }
}