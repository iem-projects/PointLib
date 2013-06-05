package at.iem.point.sh.sketches
package gui

import scala.swing.{Orientation, BoxPanel, ComboBox}
import genetic._
import de.sciss.desktop.impl.WindowImpl
import de.sciss.desktop.Window
import scala.swing.event.SelectionChanged

class EvaluationSettingsFrame() {
  var eval: Evaluation = WindowedEvaluation()

  val pane = new BoxPanel(Orientation.Vertical)

  val combo = new ComboBox[Meta[Evaluation]](Evaluation.all) {
    listenTo(selection)
    selection.item = eval.meta
    reactions += {
      case SelectionChanged(_) =>
        eval = selection.item.instance()
        val cnt = pane.contents.size
        if (cnt > 1) pane.contents.remove(1, cnt - 1)
        eval.meta.defaults.zipWithIndex.foreach { case (arg, i) =>
          arg match {
            case hm: HasMeta[_] => println("has-meta")
            case i: Int         => println("int")
            case d: Double      => println("double")
          }
        }
    }
  }

  pane.contents += combo

  new WindowImpl {
    def handler = GeneticApp.windowHandler

    protected def style = Window.Regular

    contents = pane
    pack()
    front()
  }
}