package at.iem.point.er.sketches

import swing.{BoxPanel, Orientation, Slider, Component}
import swing.event.ValueChanged
import de.sciss.synth
import synth._

class MixView(player: PlayerView) {
  private var _diskAmp = player.diskAmp
  private def diskAmp: Float = _diskAmp
  private def diskAmp_=(value: Float): Unit =
    if (_diskAmp != value) {
      _diskAmp = value
      player.diskAmp = value
    }

  private var _resynthAmp = player.diskAmp
  private def resynthAmp: Float = _resynthAmp
  private def resynthAmp_=(value: Float): Unit =
    if (_resynthAmp != value) {
      _resynthAmp = value
      player.resynthAmp = value
    }

  private final class Fader(init: Float)(setter: Float => Unit) extends Slider {
    orientation = Orientation.Vertical
    min         = -60
    max         = 12
//    value       = 0
    paintTicks  = true
    minorTickSpacing  = 2
    majorTickSpacing  = 12
    paintLabels = true
    peer.setLabelTable(peer.createStandardLabels(12))
    peer.putClientProperty("JComponent.sizeVariant", "small")

    value = (init.ampdb + 0.5f).toInt

    listenTo(this)
    reactions += {
      case ValueChanged(_) =>
//        println(s"value = $value , linear = ${value.dbamp}")
        setter(if (value > -60) value.dbamp else 0f)
    }
  }

  private lazy val ggDisk     = new Fader(diskAmp   )(diskAmp    = _)
  private lazy val ggResynth  = new Fader(resynthAmp)(resynthAmp = _)

  lazy val component: Component = new BoxPanel(Orientation.Horizontal) {
    contents += ggDisk
    contents += ggResynth
  }
}