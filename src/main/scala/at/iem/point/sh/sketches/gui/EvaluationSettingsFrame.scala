package at.iem.point.sh.sketches
package gui

import scala.swing.{Label, Component, Orientation, BoxPanel, ComboBox}
import genetic._
import de.sciss.desktop.impl.WindowImpl
import de.sciss.desktop.Window
import scala.swing.event.SelectionChanged
import javax.swing.SpinnerNumberModel
import de.sciss.guiflitz.AutoView

class EvaluationSettingsFrame() {
  var eval: Evaluation = WindowedEvaluation()

  // val pane = new BoxPanel(Orientation.Vertical)

  // pane.contents += combo

  val view = AutoView(eval)

  new WindowImpl {
    def handler = GeneticApp.windowHandler

    protected def style = Window.Regular

    contents = view.component
    pack()
    front()
  }
}