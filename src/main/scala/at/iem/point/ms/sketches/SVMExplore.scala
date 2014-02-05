package at.iem.point.ms.sketches

import scala.swing._
import libsvm.{Toy, svm}
import Swing._

object SVMExplore extends SimpleSwingApplication {
  val problems = SVM.normalize(SVM.allProblems)

  lazy val top: Frame = {
    val applet    = new Toy()
    val width     = 500
    val height    = 500 + 50
    val f         = new MainFrame {
      peer.getContentPane.add(applet)
    }
    applet.init()
    applet.setSize(width, height)
    applet.start()
    f.pack().centerOnScreen()
    f.open()

    problems.foreach { p =>
      val x +: y +: _ = p.features
      applet.addPoint(x, y, p.label)
    }
    applet.repaint()
    f
  }
}
