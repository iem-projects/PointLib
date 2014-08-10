package at.iem.point.er.sketches

import de.sciss.desktop.impl.WindowImpl
import de.sciss.desktop.Window
import de.sciss.synth
import scala.swing.Action
import de.sciss.file._

class DocumentFrame(doc: Document) extends WindowImpl {
  top =>

  val view = new DocumentView(doc)

  private lazy val onsetsSettingsFrame = OnsetsAnalysisWindow(doc)

  private lazy val pitchSettingsFrame = {
    import synth._
    val pchCfg        = PitchAnalysis.Config()
    pchCfg.input      = doc.file
    pchCfg.inputGain  =   6.dbamp
    pchCfg.ampThresh  = -48.dbamp
    pchCfg.peakThresh = -12.dbamp
    pchCfg.median     = 12
    pchCfg.stepSize   = 256 // 512
    pchCfg.maxFreqSpread = math.pow(2, 3.0/12).toFloat
    pchCfg.trajMinDur = 25.0f

    val pitchView = new PitchAnalysisSettingsView(doc, init = pchCfg)
    new WindowImpl {
      def handler = Main.windowHandler
      override def style = Window.Palette
      title = "Pitch Analysis Settings"
      // peer.getRootPane.putClientProperty("Window.style", "small")
      closeOperation = Window.CloseHide
      contents = pitchView.component
      pack()
      resizable = false
      // this.placeRightOf(top)
      front()
    }
  }

  private lazy val mixFrame = new WindowImpl {
    def handler = Main.windowHandler
    override def style = Window.Palette
    title = "Mixer"
    // peer.getRootPane.putClientProperty("Window.style", "small")
    closeOperation = Window.CloseHide
    contents = view.mixView.component
    pack()
    resizable = false
    // this.placeLeftOf(top)
    front()
  }

  def exportAsAudioFile(): Unit = {
    val tag0  = if (doc.pitches.nonEmpty) "Pitch" else ""
    val tag   = if (doc.onsets .nonEmpty) tag0 + "Onsets" else tag0
    val init  = Some(doc.createOutputPath(doc.file, tag = tag, extension = "aif"))
    GUI.saveFileDialog(init = init).foreach { f =>
      view.playerView.capture(f)
    }
  }

  def exportAsScore(): Unit = {
    if (doc.onsets.isEmpty) return
    val init  = Some(doc.createOutputPath(doc.file, tag = "Onsets", extension = "pdf"))
    GUI.saveFileDialog(tpe = "PDF Score", init = init).foreach { f =>
      ??? // ScoreExport(f, onsets, sampleRate = fileSpec.sampleRate)
    }
  }

  def exportScreenshot(): Unit = Main.pdfFun(this)

  bindMenus(
    "file.import.onsets"      -> Action(null) { onsetsSettingsFrame.load()  },
    "file.export.audiofile"   -> Action(null) { exportAsAudioFile()         },
    "file.export.score"       -> Action(null) { exportAsScore()             },
    "file.export.screenshot"  -> Action(null) { exportScreenshot()          },
    "file.export.onsets"      -> Action(null) { onsetsSettingsFrame.save()  },
    "tools.mixer"             -> Action(null) { mixFrame.front()            },
    "tools.pitch"             -> Action(null) { pitchSettingsFrame.front()  },
    "tools.onsets"            -> Action(null) { onsetsSettingsFrame.front() },
    "tools.onsets-unify"      -> Action(null) {
      val p = onsetsSettingsFrame.product
      if (p.nonEmpty) {
        val multi = MultiResOnsets(p)
        println(multi.onsets.mkString(", "))
        doc.onsets = multi
      }
    }
  )

  title     = doc.file.name
  contents  = view.component
  pack()
  // centerOnScreen()
  front()

  def handler = Main.windowHandler
}