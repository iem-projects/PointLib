package at.iem.point.er.sketches
package impl

import java.io.File
import de.sciss.desktop.impl.WindowImpl
import de.sciss.desktop.Window

import GUI.Implicits._
import de.sciss.synth.io.AudioFile
import scala.swing.BorderPanel

class OnsetsAnalysisWindowImpl(in: File) extends OnsetsAnalysisWindow with WindowImpl {
  private val fileSpec  = AudioFile.readSpec(in)
  private val oCfg      = OnsetsAnalysis.Config()
  oCfg.input    = in

  private val setView   = new OnsetsAnalysisSettingsView(inputSpec = fileSpec, init = oCfg)
  private val listView  = SettingsListView[OnsetsAnalysis.Config](_.toString)

  def handler = Main.windowHandler
  def style = Window.Palette
  title = "Onsets Analysis Settings"
  // peer.getRootPane.putClientProperty("Window.style", "small")
  closeOperation = Window.CloseHide
  contents = new BorderPanel {
    add(setView .component, BorderPanel.Position.West)
    add(listView.component, BorderPanel.Position.East)
  }
  pack()
  resizable = false
  // this.placeRightOf(top)
  front()
}