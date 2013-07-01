package at.iem.point.er.sketches
package impl

import scala.swing.{Swing, Orientation, BoxPanel, Button, ListView, BorderPanel}
import collection.immutable.{IndexedSeq => Vec}
import Swing._

class SettingsListViewImpl[A](view: A => String) extends SettingsListView[A] {
  private val listView = new ListView[A]

  private val butView = Button("\u25C0") {
    println("view")
  }

  private val butAdd = Button("+") {
    println("add")
  }

  private val butDelete = Button("\u2212") {
    println("delete")
  }

  private val butUp = Button("\u25B2") {
    println("up")
  }

  private val butDown = Button("\u25BC") {
    println("down")
  }

  private val butInterp = Button("\u2632") {
    println("interpolate")
  }

  private val buts = Vec(butView, butAdd, butDelete, butUp, butDown, butInterp)
  buts.zipWithIndex.foreach { case (b, idx) =>
    b.peer.putClientProperty("JButton.buttonType", "segmentedCapsule")
    b.peer.putClientProperty("JButton.segmentPosition",
      if (idx == 0) "first" else if (idx == buts.size - 1) "last" else "middle")
    b.preferredSize = (40, b.preferredSize.height)
  }

  private val butPane = new BoxPanel(Orientation.Horizontal) {
    contents ++= buts
  }

  val component = new BorderPanel {
    add(listView, BorderPanel.Position.Center)
    add(butPane , BorderPanel.Position.South )
  }

  def items        : Vec[A]  = listView.listData.toIndexedSeq
  def items_=(value: Vec[A]) { listView.listData = items }
}