package at.iem.point.er.sketches

import java.io.File
import de.sciss.desktop.Window
import impl.{OnsetsAnalysisWindowImpl => Impl}

object OnsetsAnalysisWindow {
  def apply(in: File): OnsetsAnalysisWindow = new Impl(in)
}
trait OnsetsAnalysisWindow extends Window