package at.iem.point.sh.sketches
package gui

import de.sciss.muta
import at.iem.point.sh.sketches.genetic.GeneticSystem
import de.sciss.muta.gui.DocumentFrame
import de.sciss.desktop.{KeyStrokes, Menu}
import java.awt.event.KeyEvent
import scala.swing.Action

object GeneticApp extends muta.gui.GeneticApp(GeneticSystem) {
  protected override def init(): Unit = {
    super.init()
    import Menu._
    import KeyStrokes._
    import KeyEvent._
    val root = menuFactory
    root.get("file.export") match {
      case Some(g: Group) =>
        g.add(
          Item("lily", proxy("Selection As Lilypond Score..." -> (menu1 + shift + VK_S)))
        )
      case _ =>
    }
  }

  override protected def configureDocumentFrame(frame: DocumentFrame[GeneticSystem.type]): Unit = {
    frame.bindMenu("file.export.lily", Action("") {
      val genome = frame.selectedNodes.map(n => n.chromosome -> n.fitness)
      ExportLilypond.dialog(info = frame.info, genome = genome, parent = Some(frame.window))
    })

    frame.bindMenu("file.export.table", Action("") {
      val genome = frame.selectedNodes.map(n => n.chromosome -> n.fitness)
      ExportTable.dialog(genome = genome, parent = Some(frame.window))
    })
  }
}