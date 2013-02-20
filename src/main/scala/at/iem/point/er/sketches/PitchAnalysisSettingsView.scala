package at.iem.point.er.sketches

import swing.{Swing, Orientation, BoxPanel, Button, BorderPanel}
import de.sciss.synth
import util.Success
import Swing._
import javax.swing.BorderFactory

class PitchAnalysisSettingsView(sono: SonogramView, sampleRate: Double,
                                init: PitchAnalysis.Config = PitchAnalysis.Config.default)
  extends BorderPanel {

  import synth._

  private val b = PitchAnalysis.ConfigBuilder(init)
  private def timeRes: Float = {
    (b.stepSize / sampleRate * 1000).toFloat
  }
  private def timeRes_=(value: Float) {
    b.stepSize = ((value * sampleRate / 1000 + 0.5).toInt).nextPowerOfTwo
    val t = timeRes
    if (math.abs(t - timeRes) > 1) setTimeRes.value = t
  }

  private def ampThresh: Float = b.ampThresh.ampdb
  private def ampThresh_=(value: Float) { b.ampThresh = value.dbamp }

  private def peakThresh: Float = b.peakThresh.ampdb
  private def peakThresh_=(value: Float) { b.peakThresh = value.dbamp }

  private def inputGain: Float = b.inputGain.ampdb
  private def inputGain_=(value: Float) { b.inputGain = value.dbamp }

  private def maxGliss: Float = b.maxFreqDev * 100
  private def maxGliss_=(value: Float) { b.maxFreqDev = value / 100 }

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
      float("Input gain:",          "dB")(inputGain  _)(inputGain = _),
      float("Maximum glissando:",   "%") (maxGliss _)(maxGliss = _),
      float("Minimum duration:",    "ms")(b.trajMinDur _)(b.trajMinDur = _)
    )
  }

  def config: PitchAnalysis.Config = b.build

  private val ggRun = Button("Run...") {
    PitchAnalysis(config) {
      case PitchAnalysis.Result(Success(seq)) =>
        sono.pitchOverlay = seq
    }
  }

  private val butPanel = new BoxPanel(Orientation.Horizontal) {
    border = BorderFactory.createEmptyBorder(8, 0, 0, 0)
    contents += HGlue
    contents += ggRun
  }

  add(GUI.Setting.vertical(settings), BorderPanel.Position.Center)
  add(butPanel, BorderPanel.Position.South)
}