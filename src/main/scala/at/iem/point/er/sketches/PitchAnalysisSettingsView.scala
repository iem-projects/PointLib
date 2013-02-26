package at.iem.point.er.sketches

import swing.{Component, Swing, Orientation, BoxPanel, Button, BorderPanel}
import de.sciss.synth
import synth.io.AudioFileSpec
import util.Success
import Swing._
import javax.swing.BorderFactory

class PitchAnalysisSettingsView(inputSpec: AudioFileSpec,
                                init: PitchAnalysis.Config = PitchAnalysis.Config.default) {
  import synth._

  private val b = PitchAnalysis.ConfigBuilder(init)
  private def timeRes: Float = {
    (b.stepSize / inputSpec.sampleRate * 1000).toFloat
  }
  private def timeRes_=(value: Float) {
    b.stepSize = ((value * inputSpec.sampleRate / 1000 + 0.5).toInt).nextPowerOfTwo
    val t = timeRes
    if (math.abs(t - timeRes) > 1) setTimeRes.value = t
  }

  private def ampThresh: Float = b.ampThresh.ampdb
  private def ampThresh_=(value: Float) { b.ampThresh = value.dbamp }

  private def peakThresh: Float = b.peakThresh.ampdb
  private def peakThresh_=(value: Float) { b.peakThresh = value.dbamp }

  private def inputGain: Float = b.inputGain.ampdb
  private def inputGain_=(value: Float) { b.inputGain = value.dbamp }

  private def maxSpread: Float = b.maxFreqSpread * 100
  private def maxSpread_=(value: Float) { b.maxFreqSpread = value / 100 }

  private def maxSlope: Float = b.maxFreqSlope * 100
  private def maxSlope_=(value: Float) { b.maxFreqSlope = value / 100 }

  private val setTimeRes = GUI.Setting.float("Time resolution:", "ms")(timeRes _)(timeRes = _)

  private val settings = {
    import GUI.Setting._
    List(
      float("Minimum frequency:", "Hz")(b.minFreq _)(b.minFreq = _),
      float("Maximum frequency:", "Hz")(b.maxFreq _)(b.maxFreq = _),
      setTimeRes,
      int("Median:")(b.median _)(b.median = _),
      float("Amplitude threshold:", "dB")(ampThresh  _)(ampThresh  = _),
      float("Peak threshold:",      "dB")(peakThresh _)(peakThresh = _),
      float("Input gain:",          "dB")(inputGain  _)(inputGain  = _),
      float("Maximum freq spread:", "%") (maxSpread  _)(maxSpread  = _),
      float("Maximum freq slope:",  "%") (maxSlope   _)(maxSlope   = _),
      float("Minimum duration:",    "ms")(b.trajMinDur _)(b.trajMinDur = _)
    )
  }

  def config: PitchAnalysis.Config = b.build

  private val ggRun = Button("Run...") {
    PitchAnalysis.verbose = true
    PitchAnalysis(config) {
      case PitchAnalysis.Result(Success(seq)) =>
//        sono.pitchOverlay = seq
      Main.pitches = seq
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