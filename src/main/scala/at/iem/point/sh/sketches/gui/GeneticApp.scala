package at.iem.point.sh.sketches
package gui

import de.sciss.muta
import at.iem.point.sh.sketches.genetic.GeneticSystem

object GeneticApp extends muta.gui.GeneticApp(GeneticSystem) {
//  protected lazy val menuFactory = {
//    import Menu._
//    import KeyStrokes._
//    import KeyEvent._
//    Root().add(
//      Group("file", "File").add(
//        Item("new")("New" -> (menu1 + VK_N)) {
//          val doc = new Document
//          new DocumentFrame(doc)
//        }
//      ).add(
//        Group("import", "Import").add(
//          Item("settings", proxy("Algorithm Settings..."      -> (menu1 + alt   + VK_O)))
//        )
//      ).add(
//        Group("export", "Export").add(
//          Item("lily", proxy("Selection As Lilypond Score..." -> (menu1 + shift + VK_S)))
//        ).add(
//          Item("settings", proxy("Algorithm Settings..."      -> (menu1 + alt   + VK_S)))
//        ).add(
//          Item("table", proxy("Selection As PDF Table..."     -> (menu1 +         VK_T)))
//        )
//      )
//    )
//  }
}