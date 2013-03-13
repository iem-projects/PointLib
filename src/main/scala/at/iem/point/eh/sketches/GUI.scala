package at.iem.point.eh.sketches

import swing.{Panel, GridPanel, Component, Frame, Swing}
import Swing._
import java.awt.Dimension
import javax.swing.WindowConstants

object GUI {
  def frame(title: String, component: Component, size: Dimension = (1000, 800)): Frame = {
    val _title    = title
    val _size     = size
    val fr = new Frame {
      title     = _title
      size      = _size
      contents  = component
      peer.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
      centerOnScreen()
    }
    PDFSupport.addMenu(fr.peer, component.peer :: Nil, usePrefSize = false)
    fr.open()
    fr
  }

  implicit final class RichComponentSequence(val sq: Iterable[Component]) /* extends AnyVal */ {
    def asGrid(rows: Int, columns: Int): Panel = new GridPanel(rows0 = 2, cols0 = 2) {
      contents ++= sq
    }
  }
}