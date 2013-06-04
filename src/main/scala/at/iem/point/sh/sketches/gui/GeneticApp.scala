package at.iem.point.sh.sketches
package gui

import de.sciss.desktop.impl.SwingApplicationImpl
import de.sciss.desktop.{KeyStrokes, Menu}
import java.awt.event.KeyEvent

object GeneticApp extends SwingApplicationImpl("Genetic Algorithm") {
  type Document = gui.Document

  protected lazy val menuFactory = {
    import Menu._
    import KeyStrokes._
    import KeyEvent._
    Root().add(
      Group("file", "File").add(
        Item("new")("New" -> (menu1 + VK_N)) {
          val doc = new Document
          new DocumentFrame(doc)
        }
      )
    )
  }
}