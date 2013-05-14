package at.iem.point.ms.sketches

import scala.swing.Swing
import GUI._
import Swing._

object Boring extends App {
  Swing.onEDT {
    val panes = Seq(true, false).flatMap { all =>
      val c1 = Kreuztabelle.analyze(Study.Boring(26), allIntervals = all)
      val c2 = Kreuztabelle.analyze(Study.Raw   ( 5), allIntervals = all)
      Seq(c1, c2)
    }
    val panel = panes.asGrid(2, 2)
    frame("Kreuztabelle", panel, (1000, 1000))
  }
}