package at.iem.point.sh.sketches
package gui

import de.sciss.guiflitz.AutoView
import de.sciss.desktop.Window
import de.sciss.desktop.impl.WindowImpl
import reflect.runtime.universe.TypeTag
import scala.swing.ScrollPane

abstract class SettingsFrame[A: TypeTag](init: A, title: String) { me =>
  final val view = AutoView(init)

  final def value       : A  = view.cell()
  final def value_=(eval: A) { view.cell() = eval }

  new WindowImpl {
    def handler     = GeneticApp.windowHandler
    def style       = Window.Regular
    title           = s"${me.title} Settings"
    closeOperation  = Window.CloseDispose
    contents        = new ScrollPane(view.component)
    pack()
    front()
  }
}