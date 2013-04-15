package at.iem.point.eh.sketches

import scala.swing.event.ValueChanged
import java.awt.event.{AdjustmentListener, AdjustmentEvent}
import scala.swing.ScrollBar

// swing.ScrollBar is not listening
class ScrollBarAlive extends ScrollBar {
  me =>
  peer.addAdjustmentListener(new AdjustmentListener {
    def adjustmentValueChanged(e: AdjustmentEvent) {
      publish(new ValueChanged(me))
    }
  })
}