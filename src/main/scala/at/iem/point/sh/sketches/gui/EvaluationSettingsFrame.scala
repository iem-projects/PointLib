package at.iem.point.sh.sketches
package gui

import scala.swing.{Label, Component, Orientation, BoxPanel, ComboBox}
import genetic._
import de.sciss.desktop.impl.WindowImpl
import de.sciss.desktop.Window
import scala.swing.event.SelectionChanged
import javax.swing.SpinnerNumberModel
import de.sciss.guiflitz.AutoView

class EvaluationSettingsFrame(init: Evaluation = WindowedEvaluation()) {
  val view = AutoView(init)

  def value       : Evaluation  = view.cell()
  def value_=(eval: Evaluation) { view.cell() = eval }

  new WindowImpl {
    def handler     = GeneticApp.windowHandler
    def style       = Window.Regular
    title           = "Evaluation Settings"
    closeOperation  = Window.CloseDispose
    contents        = view.component
    pack()
    front()
  }
}