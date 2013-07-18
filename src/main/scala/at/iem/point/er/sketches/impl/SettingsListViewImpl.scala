package at.iem.point.er.sketches
package impl

import scala.swing.{Component, ScrollPane, Swing, Orientation, BoxPanel, Button, ListView, BorderPanel}
import Swing._
import java.awt.Font
import de.sciss.swingplus.DoClickAction
import de.sciss.desktop.Implicits._
import javax.swing.KeyStroke
import java.awt.event.KeyEvent

class SettingsListViewImpl[A](cell: Cell[A], view: A => String) extends SettingsListView[A] {
  private val listView = new ListView[A]

  listView.renderer = new ListView.Renderer[A] {
    def componentFor(list: ListView[_], isSelected: Boolean, focused: Boolean, a: A, index: Int): Component = {
      ListView.GenericRenderer.componentFor(
        list, isSelected = isSelected, focused = focused, a = view(a), index = index)
    }
  }
  // listView.peer.putClientProperty("JComponent.sizeVariant", "small")
  listView.font           = new Font(Font.SANS_SERIF, Font.PLAIN, 9)
  // listView.fixedCellWidth = 130
  listView.prototypeCellValue = cell()

  def selectedItem : Option  [A  ] = listView.selection.items  .headOption
  def selectedIndex: Option  [Int] = listView.selection.indices.headOption
  def selectedItems: Iterable[A  ] = listView.selection.items

  private val butView = Button("\u25C0") {
    selectedItem.foreach(cell() = _)
  }
  // butView.focusable = false
  private val clickView = DoClickAction(butView)
  clickView.accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_LEFT, 0))
  listView.addAction("click", clickView)

  private val butAdd = Button("+") {
    items :+= cell()
  }

  private val butDelete = Button("\u2212") {
    val sel = selectedItems.toIndexedSeq
    items   = items.filterNot(sel.contains)
  }

  private def move(inc: Int): Unit = {
    val old = items
    selectedIndex.foreach { i =>
      val j = i + inc
      if (j >= 0 && j < old.size) {
        items = old.patch(i, Vec.empty, 1).patch(j, Vec(old(i)), 0)
        listView.selectIndices(j)
      }
    }
  }

  private val butUp   = Button("\u25B2") { move(-1) }
  private val butDown = Button("\u25BC") { move(+1) }

  private val butInterp = Button("\u2632") {
    println("interpolate")
  }
  butInterp.enabled = false

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

  private val scroll = new ScrollPane(listView)

  val component = new BorderPanel {
    add(scroll , BorderPanel.Position.Center)
    add(butPane, BorderPanel.Position.South )
  }

  def items        : Vec[A]         = listView.listData.toIndexedSeq
  def items_=(value: Vec[A]): Unit  = listView.listData = value
}