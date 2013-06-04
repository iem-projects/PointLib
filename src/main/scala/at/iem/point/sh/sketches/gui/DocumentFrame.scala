package at.iem.point.sh.sketches.gui

import scala.swing.Button
import de.sciss.desktop.impl.WindowImpl
import de.sciss.desktop.Window

final class DocumentFrame(val document: Document) {

  val ggDur       = new Button("Dur")
  val ggSeed      = new Button("Seed")
  val ggRandSeed  = new Button("Rand")
  val ggPop       = new Button("Pop")

  val panel =
    form"""   Duration:|$ggDur |\u2669
          |       Seed:|$ggSeed|$ggRandSeed
          | Population:|$ggPop |"""

  new WindowImpl {
    def handler = GeneticApp.windowHandler
    protected def style = Window.Regular

    contents = panel
    pack()
    front()
  }
}