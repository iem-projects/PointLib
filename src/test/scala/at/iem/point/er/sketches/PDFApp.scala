package at.iem.point.er.sketches

import de.sciss.pdflitz
import de.sciss.desktop.FileDialog
import de.sciss.file._

object PDFApp extends App {
  Main.main(null)
  Main.pdfFun = { frame =>
    val dlg = FileDialog.save(title = "Export Screenshot As PDF")
    dlg.show(Some(frame)).foreach { f =>
      pdflitz.Generate(f.replaceExt("pdf"), frame.view.view.component, overwrite = true)
    }
  }
}