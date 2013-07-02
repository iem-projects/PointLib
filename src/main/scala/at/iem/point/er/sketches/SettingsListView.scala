package at.iem.point.er.sketches

import scala.swing.Component
import impl.{SettingsListViewImpl => Impl}

object SettingsListView {
  def apply[A](cell: Cell[A])(view: A => String): SettingsListView[A] = new Impl(cell, view)
}
trait SettingsListView[A] {
  def component: Component
  var items: Vec[A]
}