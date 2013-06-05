package at.iem.point.sh.sketches
package gui

import scala.swing.{Label, Component, Orientation, BoxPanel, ComboBox}
import genetic._
import de.sciss.desktop.impl.WindowImpl
import de.sciss.desktop.Window
import scala.swing.event.SelectionChanged
import javax.swing.SpinnerNumberModel

class EvaluationSettingsFrame() {
  var eval: Evaluation = WindowedEvaluation()

  val pane = new BoxPanel(Orientation.Vertical)

  def buildMetaGUI(meta: Meta[_]): Component = {
    val p   = new BoxPanel(Orientation.Vertical)
    val lb  = new Label(s"<html><body>${meta.name}<b></b>")
    p.contents += lb
    meta.defaults.zipWithIndex.foreach { case (arg, i) =>
      val gg = arg match {
        case hm: HasMeta[_] =>
          buildMetaGUI(hm.meta)
        case i: Int =>
          val sm = new SpinnerNumberModel(i, Int.MinValue, Int.MaxValue, 1)
          val _gg = new Spinner(sm)
          _gg
        case d: Double =>
          val sm = new SpinnerNumberModel(d, Double.NegativeInfinity, Double.PositiveInfinity, 0.1)
          val _gg = new Spinner(sm)
          _gg
      }
      p.contents += gg
    }
    p
  }

  val combo = new ComboBox[Meta[Evaluation]](Evaluation.all) {
    listenTo(selection)
    selection.item = eval.meta
    reactions += {
      case SelectionChanged(_) =>
        eval = selection.item.instance()
        val cnt = pane.contents.size
        if (cnt > 1) pane.contents.remove(1, cnt - 1)
        pane.contents += buildMetaGUI(eval.meta)
        // XXX frame.pack()
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