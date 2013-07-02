package at.iem.point.er.sketches
package impl

import java.io.File
import de.sciss.desktop.impl.WindowImpl
import de.sciss.desktop.Window

import de.sciss.synth.io.AudioFile
import scala.swing.BorderPanel
import de.sciss.synth

class OnsetsAnalysisWindowImpl(in: File) extends OnsetsAnalysisWindow with WindowImpl {
  private val fileSpec  = AudioFile.readSpec(in)
  private val oCfg      = OnsetsAnalysis.Config()
  oCfg.input    = in

  private val setView   = new OnsetsAnalysisSettingsView(inputSpec = fileSpec, init = oCfg)
  private val listView  = SettingsListView[OnsetsAnalysis.ConfigAndProduct](setView) { case (cfg, _) =>
    import synth._
    f"t=${cfg.thresh}%1.2f, r=${cfg.fftSize}/${cfg.fftOverlap}, m=${cfg.median}, g=${cfg.minGap}, i=${cfg.inputGain.ampdb}%1.1f, n=${cfg.noiseFloor.ampdb}%1.1f, d=${cfg.decay}%1.1f"
  }

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