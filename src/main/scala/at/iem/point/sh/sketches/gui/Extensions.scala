package at.iem.point.sh.sketches
package gui

import scala.swing.Action
import de.sciss.desktop.FileDialog
import de.sciss.desktop.impl.WindowImpl
import de.sciss.file.File

//trait Extensions {
//  _: WindowImpl =>
//
//  // protected def selectedNodes = ???
//
//  bindMenu("file.export.lily", Action("") {
//    val nodes = selectedNodes
//    if (nodes.nonEmpty) {
//      ExportLilypond.dialog(settings, nodes.map(n => (n.chromosome, n.fitness)))
//    }
//  })
//  bindMenu("file.export.table", Action("") {
//    val nodes = selectedNodes
//    if (nodes.nonEmpty) {
//      val dlg = FileDialog.save(title = "Export Selection as PDF Table")
//      dlg.show(Some(me)).foreach(f => exportTableAsPDF(f, nodes.map(n => (n.chromosome, n.fitness))))
//    }
//  })
//
//  def exportTableAsPDF(f: File, genome: Fitness.GenomeVal) {
//    import sys.process._
//    val f1 = f.replaceExt("pdf")
//    ExportTable(f1, genome, settings)
//    Seq(pdfViewer, f1.path).!
//  }
//}