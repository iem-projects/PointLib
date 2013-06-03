package at.iem.point.sh.sketches

import scala.swing.{Component, SimpleSwingApplication}
import gui._
import java.awt.Graphics2D
import spire.syntax._

object ChromosomeViewTest extends SimpleSwingApplication {
  lazy val top = new Frame { f =>
    implicit val rng = Fitness.rng(666L)
    val sq  = Fitness.randomSequence(r"12")
    val cv  = new ChromosomeView(sq)

    contents = new Component { cmp =>
      override protected def paintComponent(g: Graphics2D) {
        cv.paint(g, cmp.width, cmp.height)
      }
      preferredSize = cv.preferredSize
    }

    this.defaultCloseOperation = Frame.CloseExit
  }
}