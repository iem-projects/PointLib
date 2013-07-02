package at.iem.point.er.sketches

import java.io.File
import de.sciss.desktop.Window
import impl.{OnsetsAnalysisWindowImpl => Impl}

object OnsetsAnalysisWindow {
  def apply(in: File): OnsetsAnalysisWindow = new Impl(in)

  type Product = Vec[OnsetsAnalysis.ConfigAndProduct]
}
trait OnsetsAnalysisWindow extends Window {
  def product: Vec[OnsetsAnalysis.ConfigAndProduct]
  def load(): Unit
  def save(): Unit
}