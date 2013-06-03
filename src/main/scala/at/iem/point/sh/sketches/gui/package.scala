package at.iem.point.sh.sketches

import scala.swing.UIElement
import scala.annotation.switch
import javax.swing.WindowConstants

package object gui {
  implicit final class RichFrame(val f: Frame) extends AnyVal {
    def defaultCloseOperation = Frame.CloseOperation(f.peer.getDefaultCloseOperation)
    def defaultCloseOperation_=(value: Frame.CloseOperation) { f.peer.setDefaultCloseOperation(value.id) }
  }

  object Frame {
    object CloseOperation {
      def apply(id: Int): CloseOperation = (id: @switch) match {
        case CloseIgnore .id => CloseIgnore
        case CloseExit   .id => CloseExit
        case CloseHide   .id => CloseHide
        case CloseDispose.id => CloseDispose
      }
    }
    sealed trait CloseOperation { def id: Int }
    case object CloseIgnore  extends CloseOperation { final val id = WindowConstants.DO_NOTHING_ON_CLOSE  }
    case object CloseExit    extends CloseOperation { final val id = WindowConstants.EXIT_ON_CLOSE        }
    case object CloseHide    extends CloseOperation { final val id = WindowConstants.HIDE_ON_CLOSE        }
    case object CloseDispose extends CloseOperation { final val id = WindowConstants.DISPOSE_ON_CLOSE     }
  }
  type Frame = swing.Frame

  implicit final class RichUIElement(val ui: UIElement) extends AnyVal {
    def width : Int = ui.peer.getWidth
    def height: Int = ui.peer.getHeight
  }
}
