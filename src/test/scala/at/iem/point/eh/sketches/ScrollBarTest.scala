//package at.iem.point.eh.sketches
//
//import scala.swing._
//
//object ScrollBarTest extends SimpleSwingApplication {
//  lazy val top = new Frame {
//    val label   = new Label { text = "0" }
//    val scroll  = new ScrollBarAlive {
//      orientation = Orientation.Horizontal
//      listenTo(this)
//      reactions += {
//        case event.ValueChanged(_) =>
//          label.text = value.toString + (if (valueIsAjusting) " A" else "")
//      }
//    }
//    contents = new BorderPanel {
//      add(label,  BorderPanel.Position.North)
//      add(scroll, BorderPanel.Position.South)
//    }
//    pack().centerOnScreen()
//    open()
//  }
//}