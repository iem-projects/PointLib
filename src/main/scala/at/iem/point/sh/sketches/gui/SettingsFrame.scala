package at.iem.point.sh.sketches
package gui

import de.sciss.guiflitz.AutoView
import de.sciss.desktop.Window
import de.sciss.desktop.impl.WindowImpl
import reflect.runtime.universe.TypeTag
import scala.swing.{Action, ScrollPane}

abstract class SettingsFrame[A: TypeTag](init: A, title: String) { me =>
  final val view = {
    val c = AutoView.Config()
    c.small = true
    AutoView(init, c)
  }

  final def value       : A  = view.cell()
  final def value_=(eval: A) { view.cell() = eval }

  new WindowImpl {
    def handler     = GeneticApp.windowHandler
    def style       = Window.Regular
    title           = s"${me.title} Settings"
    closeOperation  = Window.CloseDispose
    contents        = new ScrollPane(view.component)

    //    bindMenu("extra.refresh", Action("") {
    //      view.component.peer.invalidate()
    //      view.component.peer.revalidate()
    //      pack()
    //      view.component.peer.repaint()
    //    })

    pack()
    front()
  }
}