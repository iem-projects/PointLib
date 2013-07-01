package at.iem.point.er.sketches
package impl

import java.io.File
import de.sciss.desktop.impl.WindowImpl
import de.sciss.desktop.Window

import GUI.Implicits._
import de.sciss.synth.io.AudioFile

class OnsetsAnalysisWindowImpl(in: File) extends OnsetsAnalysisWindow with WindowImpl {
  private val fileSpec  = AudioFile.readSpec(in)
  private val oCfg      = OnsetsAnalysis.Config()
  oCfg.input    = in

  private val oView = new OnsetsAnalysisSettingsView(inputSpec = fileSpec, init = oCfg)

  def handler = Main.windowHandler
  def style = Window.Palette
  title = "Onsets Analysis Settings"
  // peer.getRootPane.putClientProperty("Window.style", "small")
  closeOperation = Window.CloseHide
  contents = oView.component
  pack()
  resizable = false
  // this.placeRightOf(top)
  front()
}