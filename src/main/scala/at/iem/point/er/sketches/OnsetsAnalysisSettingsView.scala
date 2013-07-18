package at.iem.point.er.sketches

import swing.{Component, Swing, Orientation, BoxPanel, Button, BorderPanel}
import de.sciss.synth
import util.Success
import Swing._
import javax.swing.BorderFactory
import de.sciss.processor.Processor

class OnsetsAnalysisSettingsView(doc: Document,
                                 init: OnsetsAnalysis.Config = OnsetsAnalysis.Config.default)
  extends Cell[OnsetsAnalysis.ConfigAndProduct] {
  import synth._
  import doc.{fileSpec => inputSpec}

  private val b = OnsetsAnalysis.ConfigBuilder(init)

  def timeRes: Float = {
    val stepSize = b.fftSize / b.fftOverlap
    (stepSize / inputSpec.sampleRate * 1000).toFloat
  }
  def timeRes_=(value: Float): Unit = {
    val stepSize  = (value * inputSpec.sampleRate / 1000 + 0.5).toInt.nextPowerOfTwo
    b.fftOverlap  = math.max(1, b.fftSize / stepSize)
    if (b.fftOverlap > b.fftSize) {
      setFFTSize.value = b.fftOverlap
    }
  }

  def fftSize: Int = b.fftSize
  def fftSize_=(value: Int): Unit = {
    val v = math.max(32, math.min(65536, value.nextPowerOfTwo))
    b.fftSize = v
    if (v != value) {
      setFFTSize.value = v
    }
    if (v < b.fftOverlap) {
      setTimeRes.value = (v * 1000 / inputSpec.sampleRate).toFloat
    }
  }

  def inputGain        : Float        = b.inputGain.ampdb
  def inputGain_=(value: Float): Unit = b.inputGain = value.dbamp

  def noiseFloor        : Float        = b.noiseFloor.ampdb
  def noiseFloor_=(value: Float): Unit = b.noiseFloor = value.dbamp

  private val setTimeRes = GUI.Setting.float("Time resolution:", "ms")(timeRes _)(timeRes = _)
  private val setFFTSize = GUI.Setting.int  ("FFT size:",        "ms")(fftSize _)(fftSize = _)

  private val settings = {
    import GUI.Setting._
    List(
      combo("Function:", OnsetsAnalysis.Function.seq)(b.function _)(b.function = _),
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
  def config_=(value: OnsetsAnalysis.Config): Unit = {
    b.read(value)
    settings.foreach(_.reset())
  }

  private var product = Option.empty[OnsetsAnalysis.Product]

  def apply(): OnsetsAnalysis.ConfigAndProduct = (config, product)
  def update(value: OnsetsAnalysis.ConfigAndProduct): Unit = {
    config      = value._1
    product     = value._2
    doc.onsets  = MultiResOnsets(Vec(value))
  }

  private val ggRun = ProcessorButton("Run...")(OnsetsAnalysis(config)) {
    case Success(seq) =>
      product    = Some(seq)
      doc.onsets = MultiResOnsets(Vec((config, product)))
  }

  private val butPanel = new BoxPanel(Orientation.Horizontal) {
    border    = BorderFactory.createEmptyBorder(8, 0, 0, 0)
    contents += HGlue
    contents += ggRun
  }

  lazy val component: Component = new BorderPanel {
    add(GUI.Setting.vertical(settings), BorderPanel.Position.Center)
    add(butPanel, BorderPanel.Position.South)
  }
}