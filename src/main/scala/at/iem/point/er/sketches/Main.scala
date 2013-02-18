package at.iem.point.er.sketches

import java.io.File
import annotation.tailrec
import de.sciss.sonogram.{SimpleSonogramView, SimpleSonogramOverviewManager}
import scala.Some
import javax.swing.BorderFactory
import swing.{Button, Orientation, BoxPanel, MainFrame, Component, TextField, Frame, SimpleSwingApplication, Swing}
import Swing._
import java.awt.Point
import swing.event.{MouseMoved, MouseEntered}
import de.sciss.synth
import de.sciss.dsp.ConstQ
import util.Success

object Main extends SimpleSwingApplication {
  lazy val top: Frame = {
    @tailrec def loop(): File = GUI.openAudioFileDialog() match {
      case Some(_f) => _f
      case _ => loop()
    }

    val f       = loop()
    val mgr     = new SimpleSonogramOverviewManager
    val cfg     = ConstQ.Config()
    cfg.maxFFTSize  = 8192
    cfg.maxTimeRes  = 4f
    cfg.bandsPerOct = 48
    val ov      = mgr.fromFile(f, cfg)
//    println(ov.fileSpec.sono)
    val jView   = new SonogramView
    jView.boost  = 4f
    jView.sono   = Some(ov)

    val ggStatus  = new TextField(60) {
      editable    = false
      border      = BorderFactory.createEmptyBorder()
      maximumSize = preferredSize
    }

    val ggPitch = Button("Pitch Analysis...") {
      import synth._
      val pchCfg        = PitchAnalysis.Config()
      pchCfg.input      = f
      pchCfg.inputGain  = 6.dbamp
      pchCfg.ampThresh  = -48.dbamp
      pchCfg.peakThresh = -6.dbamp
      pchCfg.median     = 12
      pchCfg.stepSize   = 256 // 512
      pchCfg.maxFreqDev = math.pow(2, 3.0/12).toFloat
      pchCfg.trajMinDur = 25.0f

      PitchAnalysis(pchCfg) {
        case PitchAnalysis.Result(Success(seq)) =>
          jView.pitchOverlay = seq
      }
    }
    ggPitch.focusable = false
    ggPitch.peer.putClientProperty("JComponent.sizeVariant", "small")

//    def sonaMouse(pt: Point, mod: Int) {
//      import synth._
//      val spc   = ov.fileSpec
//      val time  = pt.x.toDouble / jView.getWidth * spc.numFrames / spc.sampleRate // seconds
//      val freq  = (1.0 - (pt.y + 1).toDouble / jView.getHeight) * (spc.sono.maxFreq - spc.sono.minFreq) + spc.sono.minFreq  // hertz
//      ggStatus.text = f"time: $time%1.3f s, freq: $freq%1.1f Hz, pitch = ${freq.cpsmidi}%1.2f mid"
//    }

    val view = Component.wrap(jView)
    view.preferredSize = (600, 400)
//    view.listenTo(view.mouse.moves)
//    view.reactions += {
//      case MouseMoved  (_, pt, mod) => sonaMouse(pt, mod)
//      case MouseEntered(_, pt, mod) => sonaMouse(pt, mod)
//    }

    val box = new BoxPanel(Orientation.Vertical) {
      contents += view
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += ggStatus
        contents += ggPitch
      }
    }

    new MainFrame {
      title     = f.getName
      contents  = box
//      size      = (600, 400)
      pack()
      centerOnScreen()
      open()
    }
  }
}