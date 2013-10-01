package at.iem.point.ot.sketches

import de.sciss.muta
import de.sciss.desktop.{FileDialog, Menu}

object GeneticApp extends muta.gui.GeneticApp(GeneticSystem) {
  override def rowHeight = 176 // 128 // 64


  protected override def init(): Unit = {
    import Menu._
    val root = menuFactory
    root.add(Group("extra", "Extra")
      .add(Item("screenshot")("Save PDF Screenshot...")(saveScreenshot()))
    )
  }

  def saveScreenshot(): Unit =
    windowHandler.windows.toList.headOption.foreach { w =>
      val dlg = FileDialog.save()
      dlg.show(Some(w)).foreach { file =>
        val view = w.component.contents.head
        de.sciss.pdflitz.Generate(file, view)
      }
    }
}