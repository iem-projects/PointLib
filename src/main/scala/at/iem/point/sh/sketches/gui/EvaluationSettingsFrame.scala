package at.iem.point.sh.sketches
package gui

import scala.swing.ComboBox
import genetic._
import de.sciss.desktop.impl.WindowImpl
import de.sciss.desktop.Window
import scala.swing.event.SelectionChanged

class EvaluationSettingsFrame() {
  var eval  = WindowedEvaluation()

  val combo = new ComboBox[Meta[Evaluation]](Evaluation.all) {
    listenTo(selection)
    selection.item = eval.meta
    reactions += {
      case SelectionChanged(_) =>
        // eval = selection.item.instance()
    }
  }

  new WindowImpl {
    def handler = GeneticApp.windowHandler

    protected def style = Window.Regular

    contents = combo
    pack()
    front()
  }
}