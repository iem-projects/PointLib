package at.iem.point.er.sketches
package impl

import de.sciss.desktop.impl.WindowImpl
import de.sciss.desktop.{FileDialog, Window}

import de.sciss.synth.io.AudioFile
import scala.swing.BorderPanel
import de.sciss.synth
import de.sciss.file._
import play.api.libs.json.{Format, Formats, SealedTraitFormat, Json}

class OnsetsAnalysisWindowImpl(in: File) extends OnsetsAnalysisWindow with WindowImpl {
  private val fileSpec  = AudioFile.readSpec(in)
  private val oCfg      = new OnsetsAnalysis.ConfigBuilder
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

  def product = listView.items

  private def defaultConfigFile = {
    val desktop = userHome / "Desktop"
    val dir = if (desktop.isDirectory) desktop else userHome
    dir / in.replaceExt("json").path
  }

  def load() {
    val dlg = FileDialog.open(init = Some(defaultConfigFile), title = "Import Onsets Settings")
    dlg.filter = Some(_.ext.toLowerCase == "json")
    dlg.show(Some(this)).foreach { f =>
      println(f)
    }
  }

  private lazy val format: Format[Vec[OnsetsAnalysis.ConfigAndProduct]] = {
    import Formats.{FileFormat, Tuple2Format}
    import OnsetsAnalysis.{Function, Config, ConfigAndProduct}
    implicit val fmtFunc: Format[Function] = SealedTraitFormat[Function]
    implicit val fmtCfg : Format[Config  ] = SealedTraitFormat[Config  ]
    Format.GenericFormat[Vec[ConfigAndProduct]]
  }

  def save() {
    val p   = product
    if (p.isEmpty) return

    val dlg = FileDialog.save(init = Some(defaultConfigFile), title = "Export Onsets Settings")
    dlg.show(Some(this)).foreach { f =>
      implicit val fmt = format
      val js  = Json.toJson[Vec[OnsetsAnalysis.ConfigAndProduct]](product)
      val jss = Json.prettyPrint(js)
      println(jss)
    }
  }
}