package at.iem.point.er.sketches

import swing.{Component, Swing, Orientation, BoxPanel, Button, BorderPanel}
import de.sciss.synth
import synth.io.AudioFileSpec
import util.Success
import Swing._
import javax.swing.BorderFactory
import de.sciss.processor.Processor

class OnsetsAnalysisSettingsView(inputSpec: AudioFileSpec,
                                init: OnsetsAnalysis.Config = OnsetsAnalysis.Config.default) {
  import synth._

  private val b = OnsetsAnalysis.ConfigBuilder(init)
  private def timeRes: Float = {
    val stepSize = b.fftSize / b.fftOverlap
    (stepSize / inputSpec.sampleRate * 1000).toFloat
  }
  private def timeRes_=(value: Float) {
    val stepSize  = ((value * inputSpec.sampleRate / 1000 + 0.5).toInt).nextPowerOfTwo
    b.fftOverlap  = math.max(1, b.fftSize / stepSize)
    if (b.fftOverlap > b.fftSize) {
      setFFTSize.value = b.fftOverlap
    }
  }

  private def fftSize: Int = b.fftSize
  private def fftSize_=(value: Int) {
    val v = math.max(32, math.min(65536, value.nextPowerOfTwo))
    b.fftSize = v
    if (v != value) {
      setFFTSize.value = v
    }
    if (v < b.fftOverlap) {
      setTimeRes.value = (v * 1000 / inputSpec.sampleRate).toFloat
    }
  }

  private def inputGain: Float = b.inputGain.ampdb
  private def inputGain_=(value: Float) { b.inputGain = value.dbamp }

  private def noiseFloor: Float = b.noiseFloor.ampdb
  private def noiseFloor_=(value: Float) { b.noiseFloor = value.dbamp }

  private val setTimeRes = GUI.Setting.float("Time resolution:", "ms")(timeRes _)(timeRes = _)
  private val setFFTSize = GUI.Setting.int  ("FFT size:",        "ms")(fftSize _)(fftSize = _)

  private val settings = {
    import GUI.Setting._
    List(
      float("Threshold:",     "dB")(b.thresh _)(b.thresh = _),
      setTimeRes,
      setFFTSize,
      int  ("Median:"             )(b.median   _)(b.median   = _),
      int  ("Min Gap:"            )(b.minGap   _)(b.minGap   = _),
      float("Input gain:",    "dB")(inputGain  _)(inputGain  = _),
      float("Noise floor:",   "dB")(noiseFloor _)(noiseFloor = _),
      float("Decay:",         "s" )(b.decay    _)(b.decay    = _)
    )
  }

  def config: OnsetsAnalysis.Config = b.build
  def config_=(value: OnsetsAnalysis.Config) {
    b.read(value)
    settings.foreach(_.reset())
  }

  private val ggRun = Button("Run...") {
    OnsetsAnalysis.verbose = true
    OnsetsAnalysis.run(config) {
      case Processor.Result(_, Success(seq)) =>
//        seq.foreach(x => println(s"frame $x"))
        Main.onsets = seq
    }
  }

  private val butPanel = new BoxPanel(Orientation.Horizontal) {
    border = BorderFactory.createEmptyBorder(8, 0, 0, 0)
    contents += HGlue
    contents += ggRun
  }

  lazy val component: Component = new BorderPanel {
    add(GUI.Setting.vertical(settings), BorderPanel.Position.Center)
    add(butPanel, BorderPanel.Position.South)
  }
}