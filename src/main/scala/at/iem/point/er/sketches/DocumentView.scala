package at.iem.point.er.sketches

import scala.swing.{Slider, Orientation, BoxPanel, BorderPanel, Component}
import scala.swing.Swing._
import de.sciss.synth
import scala.swing.event.ValueChanged
import de.sciss.dsp.ConstQ
import de.sciss.sonogram
import de.sciss.file._
import de.sciss.audiowidgets.impl.TimelineCanvasImpl
import de.sciss.audiowidgets.TimelineModel

class DocumentView(doc: Document) {
  me =>

  val timelineModel = TimelineModel(doc.span, doc.sampleRate)

  val mcfg          = sonogram.OverviewManager.Config()
  mcfg.caching      = Some(sonogram.OverviewManager.Caching(file(sys.props("java.io.tmpdir")) / "pointlib"))
  val mgr           = sonogram.OverviewManager(mcfg)
  val cqcfg         = ConstQ.Config()
  cqcfg.maxFFTSize  = 8192
  cqcfg.maxTimeRes  = 4f
  cqcfg.bandsPerOct = 48
  val job           = sonogram.OverviewManager.Job(doc.file, cqcfg)
  val ov            = mgr.acquire(job)
  //    println(ov.fileSpec.sono)

  lazy val playerView = new PlayerView(doc)

  private lazy val ggBoost = new Slider {
    orientation = Orientation.Vertical
    min   = 0
    max   = 200
    value = 100
    //      paintTicks = true
    peer.putClientProperty("JComponent.sizeVariant", "small")
    listenTo(this)
    reactions += {
      case ValueChanged(_) =>
        import synth._
        view.sono.boost = value.linlin(0, 200, -24, 24).dbamp
    }
  }

  private object view extends TimelineCanvasImpl {
    def timelineModel = me.timelineModel

    val sono    = new SonogramView(doc, this)
    sono.boost  = 4f
    sono.sono   = Some(ov)

    def canvasComponent = Component.wrap(sono)
  }

  lazy val mixView = new MixView(playerView)

  private lazy val box = new BorderPanel {
    //    add(new BoxPanel(Orientation.Horizontal) {
    //      contents += playerView.axis
    //      contents += HStrut(ggBoost.preferredSize.width)
    //    }, BorderPanel.Position.North)
    add(view.component, BorderPanel.Position.Center)
    add(new BoxPanel(Orientation.Horizontal) {
      //        contents += ggStatus
      contents += playerView.transport
      contents += HStrut(16)
      contents += HGlue
      // contents += ggExport
      // contents += ggMix
      // contents += ggPitch
      // contents += ggOnsets
    }, BorderPanel.Position.North)
    add(ggBoost, BorderPanel.Position.East)
  }

  def component: Component = box
}