package at.iem.point.er.sketches

import scala.swing.Component
import collection.immutable.{IndexedSeq => Vec}
import impl.{SettingsListViewImpl => Impl}

object SettingsListView {
  def apply[A](view: A => String): SettingsListView[A] = new Impl(view)
}
trait SettingsListView[A] {
  def component: Component
  var items: Vec[A]
}