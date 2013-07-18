package at.iem.point.er.sketches

import scala.swing.{Swing, Slider, Orientation, BoxPanel, BorderPanel, Component}
import scala.swing.Swing._
import scala.swing.event.ValueChanged
import de.sciss.dsp.ConstQ
import de.sciss.sonogram
import de.sciss.file._
import de.sciss.audiowidgets.impl.TimelineCanvasImpl
import de.sciss.audiowidgets.TimelineModel
import de.sciss.synth._
import de.sciss.audiowidgets.j.Axis
import de.sciss.swingplus.Implicits._

class DocumentView(doc: Document) {
  me =>

  val timelineModel = TimelineModel(doc.span, doc.sampleRate)

  private val pchRange = (36, 120)

  val mcfg          = sonogram.OverviewManager.Config()
  mcfg.caching      = Some(sonogram.OverviewManager.Caching(file(sys.props("java.io.tmpdir")) / "pointlib"))
  val mgr           = sonogram.OverviewManager(mcfg)
  val cqcfg         = ConstQ.Config()
  cqcfg.maxFFTSize  = 8192
  cqcfg.maxTimeRes  = 4f
  cqcfg.bandsPerOct = 12 * 5 // 48
  cqcfg.minFreq     = (pchRange._1 - 0.5f).midicps // -0.5 because nominal low pitch should correspond to middle of lowest key!
  cqcfg.maxFreq     = (pchRange._2 - 0.5f).midicps
  val job           = sonogram.OverviewManager.Job(doc.file, cqcfg)
  val ov            = mgr.acquire(job)
  //    println(ov.fileSpec.sono)

  lazy val playerView = new PlayerView(doc, timelineModel)

  private lazy val ggBoost = new Slider {
    orientation = Orientation.Vertical
    focusable   = false

    min         = 0
    max         = 200
    value       = 100

    peer.putClientProperty("JComponent.sizeVariant", "small")
    listenTo(this)
    reactions += {
      case ValueChanged(_) =>
        view.sono.boost = value.linlin(0, 200, -24, 24).dbamp
    }
  }

  protected object view extends TimelineCanvasImpl {
    def timelineModel = me.timelineModel

    val sono    = new SonogramView(doc, this)
    sono.boost  = 4f
    sono.sono   = Some(ov)

    def canvasComponent = Component.wrap(sono)
  }

  lazy val mixView = new MixView(playerView)

  private val kb = new Keyboard
  kb.pitchRange = pchRange
  kb.keyHeight  = -1
  private lazy val kbv = {
    val res = new KeyboardView(kb)
    val top = 15 // view.canvasComponent.peer.getY
    val bot = 15 // view.component.height - view.canvasComponent.height - top
    res.border = Swing.EmptyBorder(top, 0, bot, 0)
    // res.border = Swing.MatteBorder(10, 10, 10, 10, java.awt.Color.red)
    res
  }

  private lazy val view2 = new BoxPanel(Orientation.Horizontal) {
    contents += kbv
    contents += view.component
  }

  private lazy val box = new BorderPanel {
    add(view2, BorderPanel.Position.Center)
    add(new BoxPanel(Orientation.Horizontal) {
      contents += playerView.transport
      contents += HStrut(16)
      contents += HGlue
    }, BorderPanel.Position.North)
    add(ggBoost, BorderPanel.Position.East)
  }

  def component: Component = box

  def printComponent: Component = view2
}