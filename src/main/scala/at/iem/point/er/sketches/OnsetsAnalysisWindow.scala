package at.iem.point.er.sketches

import de.sciss.desktop.Window
import impl.{OnsetsAnalysisWindowImpl => Impl}

object OnsetsAnalysisWindow {
  def apply(doc: Document): OnsetsAnalysisWindow = new Impl(doc)

  type Product = Vec[OnsetsAnalysis.ConfigAndProduct]
}
trait OnsetsAnalysisWindow extends Window {
  def product: Vec[OnsetsAnalysis.ConfigAndProduct]
  def load(): Unit
  def save(): Unit
}