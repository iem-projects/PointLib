package at.iem.point.ot.sketches

import scala.swing.{Frame, SimpleSwingApplication}
import at.iem.point.illism.chart.HarmonicFields
import scala.swing.event.WindowClosing
import de.sciss.pdflitz

object Analysis extends SimpleSwingApplication {
  lazy val top = new Frame {
    val chart = HarmonicFields.contingencyChart(chordSeq, allIntervals = true, title = "All intervals")
    contents = chart
    pack()
    centerOnScreen()
    open()

    reactions += {
      case WindowClosing(_) => quit()
    }

    new pdflitz.SaveAction(chart :: Nil).setupMenu(this)
  }
}